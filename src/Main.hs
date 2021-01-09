{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative (Alternative, empty)
import Control.Concurrent (forkIO)
import Control.Lens hiding (Context)
import Control.Monad (guard)
import Control.Monad.Except (runExceptT, ExceptT(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (withReaderT, ReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bool (bool)
import Data.Functor (void)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Yaml as Y
import Discord
import Discord.Types
import qualified Discord.Requests as R
import System.Exit (exitFailure, exitSuccess)
import System.IO (stderr)
import UnliftIO.Environment (getEnv, lookupEnv)
import UnliftIO.STM

import qualified Curve.Config as C
import qualified Curve.Database as D
import qualified Curve.Options as O
import qualified Curve.TH as TH

data Context = Context
  { _config   :: !C.Config
  , _dbConn   :: !D.Connection
  , _newJoins :: !(TVar (Map GuildId (Set UserId)))
  , _botId    :: !(TMVar UserId)
  , _handle   :: !DiscordHandle
  }
makeLenses ''Context

type CurveM = ReaderT Context IO

main :: IO ()
main = do
  tok  <- T.pack <$> getEnv "DISCORD_TOKEN"
  cmd <- O.command
  case cmd of
    O.Run -> curve tok
    O.Version -> T.putStrLn $ "Curve v" <> $(TH.cabalVersion)
    O.FillGuild (Just gid) -> fillGuild tok gid
    O.FillGuild Nothing -> T.hPutStrLn stderr "Cannot parse guild ID"

fillGuild :: Text -> GuildId -> IO ()
fillGuild tok gid = void $ runDiscord def
  { discordToken = tok
  , discordOnStart = onStart
  }
  where
  onStart = do
    mbrs <- runExceptT $ fullMemberList gid
    liftIO case mbrs of
      Left err -> do
        T.hPutStrLn stderr . T.pack . show $ err
        exitFailure
      Right mbrs' -> do
        D.withConnection \conn -> D.markReturnings conn (userId . memberUser <$> mbrs') $ repeat gid
        exitSuccess

curve :: Text -> IO ()
curve tok = do
  confFile <- fromMaybe "curve.yml" <$> lookupEnv "CURVE_CONFIG"
  conf     <- Y.decodeFileThrow confFile
  newjoins <- newTVarIO M.empty
  bot      <- newEmptyTMVarIO
  D.withConnection \conn ->
    let context = withReaderT $ Context conf conn newjoins bot
        discordConfig = def
          { discordToken = tok
          , discordOnStart = context startHandler
          , discordOnEnd = T.hPutStrLn stderr "Disconnected"
          , discordOnEvent = context . eventHandler
          , discordOnLog = T.hPutStrLn stderr
          }
     in runDiscord discordConfig >>= T.hPutStrLn stderr

startHandler :: CurveM ()
startHandler = do
  Right self <- disc $ restCall R.GetCurrentUser
  view botId >>= atomically . flip putTMVar (userId self)
  liftIO $ T.hPutStrLn stderr "Connected"

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
  { reactionGuildId = g, reactionUserId = uid, reactionMessageId = mid, reactionEmoji = e } =
  void $ runMaybeT do
    gid      <- hoistMaybe g
    allNew   <- view newJoins >>= readTVarIO
    newjoins <- hoistMaybe (allNew ^. at gid)
    guard $ uid `S.member` newjoins
    conn     <- view dbConn
    new      <- not <$> D.isReturning conn uid gid
    conf     <- view config
    eid      <- hoistMaybe $ emojiId e
    settings <- hoistMaybe $ conf ^. C.reactMessages . at mid
    res      <- hoistMaybe $ settings ^. C.responses . at eid
    let status = bool "returning" "new" new
        msg    = "Welcome " <> status <> " " <> res <> "! <@" <> (T.pack . show) uid <> ">"
        chan   = settings ^. C.responseChannel
    lift $ leaveHandler uid gid
    void . liftIO . forkIO $ D.markReturning conn uid gid
    void . lift . disc . restCall $ R.CreateMessage chan msg

messageHandler :: Message -> CurveM ()
messageHandler m = do
  bot <- view botId >>= atomically . readTMVar
  disc $ case (pinged bot, special) of
    (True, True)  -> void . restCall $ R.CreateMessage (messageChannel m) "Hello :eyes:"
    (True, False) -> void . restCall $ R.CreateReaction (messageChannel m, messageId m) "eyes"
    _             -> pure ()
  where
  pinged u = u `elem` (userId <$> messageMentions m)
  special = (userId . messageAuthor) m == 263665073577263104

joinHandler :: UserId -> GuildId -> CurveM ()
joinHandler uid gid = view newJoins >>= atomically . flip modifyTVar' adduser
  where adduser = at gid %~ Just . maybe (S.singleton uid) (S.insert uid)

leaveHandler :: UserId -> GuildId -> CurveM ()
leaveHandler uid gid = view newJoins >>=
  atomically . flip modifyTVar' (at gid %~ fmap (sans uid))

disc :: ReaderT DiscordHandle m a -> ReaderT Context m a
disc = withReaderT $ view handle

hoistMaybe :: Alternative f => Maybe a -> f a
hoistMaybe = maybe empty pure
