module Curve.Options where

import Data.Bool (bool)
import Data.Foldable (fold)
import Discord.Types (GuildId)
import Options.Applicative
import Text.Read (readMaybe)

data Command
  = Run
  | Version
  | FillGuild !(Maybe GuildId)

command :: IO Command
command = execParser . info parser $ fold []

parser :: Parser Command
parser = fillParser <|> versionParser

fillParser :: Parser Command
fillParser = FillGuild . readMaybe <$> (strOption . fold $
  [ long "fillGuild"
  , short 'f'
  , metavar "GUILD"
  , help "Put current guild members into database"
  ])

versionParser :: Parser Command
versionParser = bool Run Version <$> (switch . fold $
  [ long "version"
  , short 'v'
  , help "Show version info"
  ])
