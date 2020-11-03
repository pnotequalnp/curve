{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Lens hiding (Context)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (withReaderT, ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bool (bool)
import Data.Functor (void)
import Data.IORef
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
  , _newJoins :: !(IORef (Set UserId))
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
  newjoins <- newIORef S.empty
  bot      <- newIORef (undefined :: UserId)
  dbInit conn
  let context = withReaderT $ Context conf conn newjoins bot
      discordConfig = def
        { discordToken = tok
        , discordOnStart = context startHandler
        , discordOnEnd = putStrLn "Disconnected"
        , discordOnEvent = context . eventHandler
        , discordOnLog = T.putStrLn
        }
  runDiscord discordConfig >>= T.putStrLn
  Sql.close conn

dbConnect :: IO Sql.Connection
dbConnect = do
  host <- getEnv "POSTGRES_HOST"
  user <- getEnv "POSTGRES_USER"
  pass <- getEnv "POSTGRES_PASSWORD"
  db   <- getEnv "POSTGRES_DB" <|> pure ""
  Sql.connect $ Sql.ConnectInfo host 5432 user pass db

dbInit :: Sql.Connection -> IO ()
dbInit conn =
  void $ Sql.execute_ conn "CREATE TABLE IF NOT EXISTS users (id VARCHAR(20) PRIMARY KEY)"

startHandler :: CurveM ()
startHandler = do
  Right self <- disc $ restCall R.GetCurrentUser
  view botId >>= liftIO . flip atomicModifyIORef (const (userId self, ()))
  liftIO $ putStrLn "Connected"

eventHandler :: Event -> CurveM ()
eventHandler = \case
  GuildMemberAdd _ gu   -> joinHandler . userId $ memberUser gu
  GuildMemberRemove _ u -> leaveHandler $ userId u
  MessageCreate m       -> messageHandler m
  MessageReactionAdd r  -> reactionHandler r
  _                     -> pure ()

reactionHandler :: ReactionInfo -> CurveM ()
reactionHandler ReactionInfo { reactionUserId = u, reactionMessageId = m, reactionEmoji = e } =
  void $ runMaybeT do
    newjoins <- view newJoins >>= liftIO . readIORef
    guard $ u `S.member` newjoins
    conn     <- view dbConn
    usrs     <- liftIO $ Sql.query conn select userid
    conf     <- view config
    eid      <- hoistMaybe $ emojiId e
    settings <- hoistMaybe $ conf ^. C.reactMessages . at m
    res      <- hoistMaybe $ settings ^. C.responses . at eid
    let status = bool "returning" "new" $ null (usrs :: [[String]])
        msg    = "Welcome " <> status <> " " <> res <> "! <@" <> (T.pack . show) u <> ">"
        chan   = settings ^. C.responseChannel
    _ <- lift . disc . restCall $ R.CreateMessage chan msg
    lift $ leaveHandler u
    liftIO $ Sql.execute conn insert userid
  where
  select = "SELECT id FROM users WHERE id = ?"
  insert = "INSERT INTO users (id) VALUES(?) ON CONFLICT DO NOTHING"
  userid = Sql.Only . show $ u

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

joinHandler :: UserId -> CurveM ()
joinHandler uid = view newJoins >>= liftIO . flip atomicModifyIORef (S.insert uid &&& const ())

leaveHandler :: UserId -> CurveM ()
leaveHandler uid = view newJoins >>=
  liftIO . flip atomicModifyIORef (sans uid &&& const ())

disc :: ReaderT DiscordHandle m a -> ReaderT Context m a
disc = withReaderT $ view handle

hoistMaybe :: Monad m => Maybe a -> MaybeT m a
hoistMaybe = MaybeT . pure
