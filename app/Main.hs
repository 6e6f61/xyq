module Main where

import Paths_xyq (version)
import Data.Version (showVersion)
import Data.Semigroup ((<>))
import Options.Applicative
import Control.Monad

import Lib.Json
import qualified Lib

data Opts = Opts
    { verbose :: !Bool
    , subcommand :: !Command
    }

data Command
    = Value String
    | Key String
    | Dump
    -- | Delete

data Error
  = ParsingError Lib.Error
  | Unknown
  | Missing
  deriving (Show)

-- I doubt this is proper but it'll do
wrapParseError :: Either Lib.Error Lib.Value -> Either Error Lib.Value
wrapParseError (Left x)  = Left $ ParsingError x
wrapParseError (Right x) = Right x

main :: IO ()
main = do
  stdin <- getContents
  opts  <- execParser optsParser
  main' stdin opts (subcommand opts)

main' stdin _ Dump = print $ identAndParse stdin
main' stdin opts (Value value) = handleApply Lib.find value stdin opts
main' stdin opts (Key key) = handleApply Lib.find' key stdin opts

handleApply x y stdin opts =
  case x y <$> identAndParse stdin of
    Right (Just x) -> putStrLn x
    Right Nothing  -> printV "No match"
    Left Unknown   -> printV "Couldn't identify data type"
    Left x         -> printV ("Other unspecified error: " ++ show x)
    where printV = when (verbose opts) . putStrLn

identAndParse :: String -> Either Error Lib.Value
identAndParse x
  | isJson x = wrapParseError $ snd <$> Lib.runParser parseJson x
  | otherwise = Left Unknown

optsParser :: ParserInfo Opts
optsParser =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "it's kinda like jq")

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version)
  (long "version" <> help "Show version")

programOptions :: Parser Opts
programOptions
  = Opts
  <$> switch
    ( long "verbose"
    <> short 'v'
    <> help "Show errors instead of failing quietly" )
  <*> hsubparser (valueCommand <> dumpCommand <> keyCommand)

dumpCommand :: Mod CommandFields Command
dumpCommand
  = command "dump"
    (info (pure Dump) (progDesc "Dump the parsed AST"))

valueCommand :: Mod CommandFields Command
valueCommand
  = command "value"
    (info valueOptions (progDesc "Find the full path to a value by its key"))
      
valueOptions :: Parser Command
valueOptions = Value <$> strArgument (metavar "key")

keyCommand :: Mod CommandFields Command
keyCommand
  = command "key"
    (info keyOptions (progDesc "Find the full path to a key by its name"))

keyOptions :: Parser Command
keyOptions = Key <$> strArgument (metavar "key")