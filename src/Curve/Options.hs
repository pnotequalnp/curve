module Curve.Options where

import Data.Foldable (fold)
import Discord.Types (GuildId)
import Options.Applicative

data Options = Options
  { fillGuild :: !(Maybe GuildId)
  }

options :: IO Options
options = execParser . info parser $ fold []

parser :: Parser Options
parser = toOptions <$> optParser
  where
  toOptions = Options . \case
    "" -> Nothing
    x  -> Just $ read @GuildId x
  optParser = strOption . fold $
    [ long "fillGuild"
    , short 'f'
    , metavar "GUILD"
    , help "Put current guild members into database"
    , value ""
    ]
