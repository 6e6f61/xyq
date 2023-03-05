module Lib.Json where

import Lib
import Data.Char
import Control.Applicative
import qualified Data.Map as Map

data Value
  = Bool Bool
  | Number Integer -- TODO: Floats
  | String String
  | Array [Value]
  | Object (Map.Map String Value)
  | Null
  deriving (Show, Eq)

-- This is extremely primitive
isJson :: [Char] -> Bool
isJson x
  = case json of
  '{':xs -> True
  _      -> False
  where json = dropWhile isSpace x

seperator :: Parser Char
seperator = parseDelimiter ','

parseJson :: Parser Value
parseJson = parseObject

parseValue :: Parser Value
parseValue
  =   (Bool   <$> parseBool)
  <|> (Number <$> parseInteger)
  <|> (String <$> parseString)
  <|> (Null   <$  parseNull)
  <|> parseArray

parseObject :: Parser Value
parseObject
  = Object . Map.fromList <$> (
     parseDelimiter '{'
  *> seperated seperator pair
  <* parseDelimiter '}')
  where
    pair = (\k _ v -> (k, v)) <$> parseString <*> parseDelimiter ':' <*> parseValue

parseArray :: Parser Value
parseArray = Array <$> (parseDelimiter '[' *> elements <* parseDelimiter ']')
  where
    elements = seperated seperator parseValue

parseNull :: Parser Value
parseNull = Null <$ parseLiteral "null"