{-# LANGUAGE LambdaCase #-}

module Main where

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
stringP [] = Parser $ const Nothing
stringP (x : xs) = Parser $ \case
  y : ys
    | y == x ->
      ( \case
          Nothing -> Just (ys, [x])
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

jsonNull :: Parser JsonValue
jsonNull = fmap (const JsonNull) (stringP "NULL")

main :: IO ()
main = undefined