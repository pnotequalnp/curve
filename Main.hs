{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens hiding (Context)
import Control.Monad (guard)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (withReaderT, ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bool (bool)
import Data.Functor (void)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import qualified Database.PostgreSQL.Simple as Sql
import Discord
import Discord.Types
import qualified Discord.Requests as R
import System.Environment (getEnv)

import qualified Config as C

data Context = Context
  { _config   :: !C.Config
  , _dbConn   :: !Sql.Connection
  , _newJoins :: !(IORef (Map GuildId (Set UserId)))
  , _botId    :: !(IORef UserId)
  , _handle   :: !DiscordHandle
  }
makeLenses ''Context

type CurveM = ReaderT Context IO

main :: IO ()
main = do
  tok      <- T.pack <$> getEnv "DISCORD_TOKEN"
  confFile <- getEnv "CURVE_CONFIG" <|> pure "curve.yml"
  conf     <- Y.decodeFileThrow confFile
  conn     <- dbConnect
  newjoins <- newIORef M.empty
  bot      <- newIORef (undefined :: UserId)
  dbInit conn
  let context = withReaderT $ Context conf conn newjoins bot
      discordConfig = def
        { discordToken = tok
        , discordOnStart = context startHandler
        , discordOnEnd = Sql.close conn *> putStrLn "Disconnected"
        , discordOnEvent = context . eventHandler
        , discordOnLog = T.putStrLn
        }
  runDiscord discordConfig >>= T.putStrLn

dbConnect :: IO Sql.Connection
dbConnect = do
  host <- getEnv "POSTGRES_HOST"
  user <- getEnv "POSTGRES_USER"
  pass <- getEnv "POSTGRES_PASSWORD"
  port <- getEnv "POSTGRES_PORT"
  db   <- getEnv "POSTGRES_DB" <|> pure ""
  Sql.connect $ Sql.ConnectInfo host (read port) user pass db

dbInit :: Sql.Connection -> IO ()
dbInit conn =
  void $ Sql.execute_ conn create
  where
  create =
    "CREATE TABLE IF NOT EXISTS users (\
    \  id VARCHAR(20),\
    \  guild_id VARCHAR(20),\
    \  PRIMARY KEY (id, guild_id)\
    \)"

startHandler :: CurveM ()
startHandler = do
  Right self <- disc $ restCall R.GetCurrentUser
  view botId >>= liftIO . flip atomicModifyIORef (const (userId self, ()))
  liftIO $ putStrLn "Connected"

fullMemberList :: GuildId -> ExceptT RestCallErrorCode DiscordHandler [GuildMember]
fullMemberList guildid = go Nothing
  where
  go lowest = do
    members <- ExceptT . restCall . R.ListGuildMembers guildid $
      R.GuildMembersTiming (Just 1000) lowest
    if length members < 1000
      then pure members
      else (members <>) <$> go (Just . userId . memberUser . last $ members)

eventHandler :: Event -> CurveM ()
eventHandler = \case
  GuildMemberAdd gid gu   -> joinHandler (userId . memberUser $ gu) gid
  GuildMemberRemove gid u -> leaveHandler (userId u) gid
  MessageCreate m         -> messageHandler m
  MessageReactionAdd r    -> reactionHandler r
  _                       -> pure ()

reactionHandler :: ReactionInfo -> CurveM ()
reactionHandler ReactionInfo
  { reactionGuildId = g, reactionUserId = u, reactionMessageId = m, reactionEmoji = e } =
  void $ runMaybeT do
    gid      <- hoistMaybe g
    allNew   <- view newJoins >>= liftIO . readIORef
    newjoins <- hoistMaybe (allNew ^. at gid)
    guard $ u `S.member` newjoins
    let guildid = show gid
    conn     <- view dbConn
    usrs     <- liftIO $ Sql.query conn select (userid, guildid)
    conf     <- view config
    eid      <- hoistMaybe $ emojiId e
    settings <- hoistMaybe $ conf ^. C.reactMessages . at m
    res      <- hoistMaybe $ settings ^. C.responses . at eid
    let status = bool "returning" "new" $ null (usrs :: [Sql.Only String])
        msg    = "Welcome " <> status <> " " <> res <> "! <@" <> (T.pack . show) u <> ">"
        chan   = settings ^. C.responseChannel
    _ <- lift . disc . restCall $ R.CreateMessage chan msg
    lift $ leaveHandler u gid
    liftIO $ Sql.execute conn insert (userid, guildid)
  where
  select = "SELECT id FROM users WHERE id = ? AND guild_id = ?"
  insert = "INSERT INTO users (id, guild_id) VALUES(?,?) ON CONFLICT DO NOTHING"
  userid = show u

messageHandler :: Message -> CurveM ()
messageHandler m = do
  bot <- view botId >>= liftIO . readIORef
  disc $ case (pinged bot, special) of
    (True, True)  -> void . restCall $ R.CreateMessage (messageChannel m) "Hello :eyes:"
    (True, False) -> void . restCall $ R.CreateReaction (messageChannel m, messageId m) "eyes"
    _             -> pure ()
  where
  pinged u = u `elem` (userId <$> messageMentions m)
  special = (userId . messageAuthor) m == 263665073577263104

joinHandler :: UserId -> GuildId -> CurveM ()
joinHandler uid gid = view newJoins >>= liftIO . flip atomicModifyIORef (adduser &&& const ())
  where
  adduser :: Map GuildId (Set UserId) -> Map GuildId (Set UserId)
  adduser = at gid %~ f
  f :: Maybe (Set UserId) -> Maybe (Set UserId)
  f s = fmap (S.insert uid) s <|> Just (S.singleton uid)

leaveHandler :: UserId -> GuildId -> CurveM ()
leaveHandler uid gid = view newJoins >>=
  liftIO . flip atomicModifyIORef (at gid %~ fmap (sans uid) &&& const ())

disc :: ReaderT DiscordHandle m a -> ReaderT Context m a
disc = withReaderT $ view handle

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
