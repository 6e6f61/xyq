module Lib.Json (isJson, parseJson) where

import Lib
import Data.Char
import qualified Data.Map as Map
import Control.Applicative

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
  <|> parseObject

parseObject :: Parser Value
parseObject
  = Object <$> (
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