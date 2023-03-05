module Lib where

import Data.Char
import Data.Tuple
import Control.Applicative
import Control.Monad

import Debug.Trace

data Value
  = Bool Bool
  | Number Integer -- TODO: Floats
  | String String
  | Array [Value]
  | Object [(String, Value)]
  | Null
  deriving (Show)

data Error
  = Eof   String
  | Wrong (String, String)
  | Empty String
  | NoMatch String
  deriving (Show)

newtype Parser a = Parser
  { runParser :: String -> Either Error (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser v
    where
      v input = do
        (input', x) <- p input
        Right (input', f x)

instance Applicative Parser where
  pure x = Parser $ \i -> Right (i, x)
  (Parser p1) <*> (Parser p2)
    = Parser $ \input -> do
      (input', f)  <- p1 input
      (input'', a) <- p2 input'
      Right (input'', f a)

instance Alternative Parser where
  empty = Parser $ \input -> Left (NoMatch input)
  (Parser p1) <|> (Parser p2)
    = Parser $ \input -> p1 input <> p2 input

-- Code quarantine 

-- |Return a string representation of the location of the key holding the given value.
find :: String -> Value -> Maybe String
find needle = f ""
  where
    f path (String s)  = if s == needle then Just path else Nothing
    f path (Array xs)  = msum $ zipWith (\index value -> f (path ++ "[" ++ show index ++ "]") value) [0..] xs
    f path (Object xs) = msum $ map (\(key, value) -> f (path ++ "." ++ key) value) xs
    f path _           = Nothing

-- |Like find, but searches for a key instead of a value.
find' :: String -> Value -> Maybe String
find' x y = f x y ""
  where
    f matchK (Array r@(x:xs)) path =
      case find' matchK x of
        -- TODO: Bug here, array index is calculated incorrectly.
        Just p  -> Just (path ++ "[" ++ show (length r - length xs) ++ "]" ++ p)
        Nothing -> f matchK (Array xs) path

    f matchK (Object ((k, v):xs)) path
      | matchK == k = Just ("." ++ k)
      | otherwise =
        case find' matchK v of
          Just p  -> Just ("." ++ k ++ p)
          Nothing -> f matchK (Object xs) ("." ++ k ++ path)

    f _ _ _ = Nothing

-- /

-- |Convert empty input into an Empty Error
must :: Parser [a] -> Parser [a]
must (Parser p) = Parser $ \input -> do
  (input', xs) <- p input
  if null xs
    then Left $ NoMatch $ show input
    else Right (input', xs)

-- |Return a parser that parses a list of `b` seperated by `a`.
seperated :: Parser a -> Parser b -> Parser [b]
seperated seperator element
  =   (:) <$> element <*> many (seperator *> element)
  <|> pure []

-- |Match the first character of input
parseChar :: Char -> Parser Char
parseChar x = Parser f
  where
    f (y:ys)
      | y == x    = Right (ys, y)
      | otherwise = Left $ Wrong ("expected: " ++ [x], "got: " ++ [y]) -- Temporary TM
    f [] = Left $ Eof ("expected: " ++ [x])

-- |Parse a single character, potentially surrounded by whitespace.
parseDelimiter :: Char -> Parser Char
parseDelimiter x = whitespace *> parseChar x <* whitespace

-- |Match a sequence of characters in the input
parseLiteral :: String -> Parser String
parseLiteral = traverse parseChar

whitespace :: Parser String
whitespace = parseSpan isSpace

-- TODO: Support string escaping

-- |Match and drop a ", any number of non-" characters, then another ".
parseString :: Parser String
parseString = parseChar '"' *> parseSpan (/= '"') <* parseChar '"'

-- |Like `parseString`, but expects ' delimited strings.
parseString' :: Parser String
parseString' = parseChar '\'' *> parseSpan (/= '\'') <* parseChar '\''

-- |Combines `parseString` and `parseString'`
parseString'' :: Parser String
parseString'' = parseString <|> parseString'

-- |Match the string "true" or "false", case-sensitive.
parseBool :: Parser Bool
parseBool = f <$> (parseLiteral "true" <|> parseLiteral "false")
  where
    f "true"  = True
    f "false" = False

-- |Match the string "True" or "False", case-sensitive.
parseBool' :: Parser Bool
parseBool' = f <$> (parseLiteral "True" <|> parseLiteral "False")
  where
    f "True" = True
    f "False" = False

-- |Match a span of characters matching the predicate
parseSpan :: (Char -> Bool) -> Parser String
parseSpan f = Parser f'
  where f' input = Right (swap $ span f input)

-- |Match a span of numbers
parseInteger :: Parser Integer
parseInteger = read <$> must (parseSpan isDigit)