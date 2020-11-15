module Curve.Database
  ( Connection
  , isReturning
  , markReturning
  , withConnection
  ) where

import Control.Monad.IO.Unlift
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Database.PostgreSQL.Simple hiding (connect)
import qualified Database.PostgreSQL.Simple as Sql
import Discord.Types
import UnliftIO.Environment (getEnv, lookupEnv)
import UnliftIO.Exception (bracket)

connect :: MonadIO m => m Connection
connect = do
  host <- getEnv "POSTGRES_HOST"
  user <- getEnv "POSTGRES_USER"
  pass <- getEnv "POSTGRES_PASSWORD"
  port <- getEnv "POSTGRES_PORT"
  db   <- fromMaybe "" <$> lookupEnv "POSTGRES_DB"
  liftIO . Sql.connect $ ConnectInfo host (read port) user pass db

initialize :: MonadIO m => Connection -> m Connection
initialize conn = do
  void . liftIO $ execute_ conn
    "CREATE TABLE IF NOT EXISTS users (\
    \  id VARCHAR(20),\
    \  guild_id VARCHAR(20),\
    \  PRIMARY KEY (id, guild_id)\
    \)"
  pure conn

withConnection :: MonadUnliftIO m => (Connection -> m a) -> m a
withConnection m = withRunInIO \unlift ->
  bracket (connect >>= initialize) close (unlift . m)

isReturning :: MonadIO m => Connection -> UserId -> GuildId -> m Bool
isReturning conn userid guildid = do
  usrs <- liftIO $ query conn select (show userid, show guildid)
  pure . not . null $ (usrs :: [Only String])
  where select = "SELECT id FROM users WHERE id = ? AND guild_id = ?"

markReturning :: MonadIO m => Connection -> UserId -> GuildId -> m ()
markReturning conn userid guildid = void . liftIO $ execute conn insert (show userid, show guildid)
  where insert = "INSERT INTO users (id, guild_id) VALUES(?,?) ON CONFLICT DO NOTHING"
