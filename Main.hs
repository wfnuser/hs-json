{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

data JsonValue
  = JsonNull
  | JsonBool Bool
  deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue = jsonNull

charP :: Char -> Parser Char
charP x = Parser $ \case
  y : ys
    | y == x -> Just (ys, x)
  _ -> Nothing

-- How to use charP to construct stringP
stringP :: String -> Parser String
stringP [] = Parser $ \input -> Just (input, "")
stringP (x : xs) = Parser $ \case
  y : ys
    | y == x ->
      ( \case
          Nothing -> Nothing
          Just (s, a) -> Just (s, x : a)
      )
        (runParser (stringP xs) ys)
    | otherwise -> Nothing
  _ -> Nothing

-- we can use fmap to do that;
-- just map string "NULL" to JsonNull
-- jsonNull :: Parser JsonValue
-- jsonNull = Parser $ \input -> (\case
--                 Just (s, a) -> Just (s, JsonNull)
--                 Nothing -> Nothing
--             ) (runParser (stringP "NULL") input)

-- first of all; we need to prove that Parser is a Functor
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (s, a) -> Just (s, f a)
      Nothing -> Nothing

-- Parser (a->b) :: string -> (string, (a->b))
-- Parser a :: string -> (string, a)
instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input1, f) <- p1 input
    (input2, a) <- p2 input1
    return (input2, f a)

instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = fmap (const JsonNull) (stringP "null")

jsonBool :: Parser JsonValue
-- jsonBool = jsonTrue <|> jsonFalse
jsonBool =
  Parser $
    \input ->
      case runParser jsonTrue input of
        Just (s, a) -> Just (s, a)
        _ -> runParser jsonFalse input
  where
    jsonTrue = fmap (const $ JsonBool True) (stringP "true")
    jsonFalse = fmap (const $ JsonBool False) (stringP "false")

main :: IO ()
main = undefined