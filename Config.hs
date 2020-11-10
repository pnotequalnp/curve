{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Config where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Data.Text (Text)
import Discord.Types (ChannelId, MessageId, EmojiId)
import Discord.Internal.Types (Snowflake(..))

deriving instance ToJSONKey Snowflake
deriving instance FromJSONKey Snowflake

data ReactionMessage = ReactionMessage
  { _responseChannel :: !ChannelId
  , _responses :: !(Map EmojiId Text)
  } deriving Show
makeLenses ''ReactionMessage
deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''ReactionMessage

data Config = Config
    { _reactMessages :: !(Map MessageId ReactionMessage)
    } deriving Show
makeLenses ''Config
deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''Config
