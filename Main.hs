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
    | y == x -> (\case
        Nothing -> Just (ys, [x]) 
        Just (s, a) -> Just (s, x:a)
    ) (runParser (stringP xs) ys)
    | otherwise -> Nothing
  _ -> Nothing


jsonNull :: Parser JsonValue
jsonNull = Parser $ \input -> (\case
                Just (s, a) -> Just (s, JsonNull)
                Nothing -> Nothing 
            ) (runParser (stringP "NULL") input)


main :: IO ()
main = undefined