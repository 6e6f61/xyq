module Main where

import Paths_xyq (version)
import Data.Version (showVersion)
import Data.Semigroup ((<>))
import Options.Applicative

data Opts = Opts
    { verbose :: !Bool
    , subcommand :: !Command
    }

data Command
    = Where String
    -- | Delete

main :: IO ()
main = do
    opts <- execParser optsParser
    case subcommand opts of
        Where is -> print is
        -- Delete -> putStrLn "Deleted the thing!"

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "it's kinda like jq")

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version)
  (long "version" <> short 'v' <> help "Show version")

programOptions :: Parser Opts
programOptions
  = Opts
  <$> switch
    ( long "verbose"
    <> help "Show errors instead of failing quietly" )
  <*> hsubparser (whereCommand)
  
whereCommand :: Mod CommandFields Command
whereCommand
  = command "where"
    (info whereOptions (progDesc "Find the full path or index of a key"))
      
whereOptions :: Parser Command
whereOptions
  = Where
  <$> strArgument (metavar "IS")
  
-- deleteCommand :: Mod CommandFields Command
-- deleteCommand =
--     command
--         "delete"
--         (info (pure Delete) (progDesc "Delete the thing"))
