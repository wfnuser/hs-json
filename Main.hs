{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Applicative
import Data.Char

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Int
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

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

-- Before we prove Parser is an alternative, we need to prove it is an applicative.
-- And why is that? 🤔
instance Alternative Parser where
  empty = Parser $ const empty
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

jsonNull :: Parser JsonValue
jsonNull = fmap (const JsonNull) (stringP "null")

jsonBool :: Parser JsonValue
jsonBool =
  jsonTrue <|> jsonFalse
  where
    --   Parser $
    --     \input ->
    --       case runParser jsonTrue input of
    --         Just (s, a) -> Just (s, a)
    --         _ -> runParser jsonFalse input

    jsonTrue = fmap (const $ JsonBool True) (stringP "true")
    jsonFalse = fmap (const $ JsonBool False) (stringP "false")

jsonNumber :: Parser JsonValue
jsonNumber = fmap (JsonNumber . read) (spanP isDigit)

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (hit, remain) = span f input
   in case hit of
        "" -> Nothing
        _ -> Just (remain, hit)

isNotQuotation :: Char -> Bool
isNotQuotation '"' = False
isNotQuotation _ = True

isNotColon :: Char -> Bool
isNotColon ':' = False
isNotColon _ = True

jsonString :: Parser JsonValue
jsonString = Parser $ \input -> do
  (remain, c) <- runParser (charP '"') input
  (remain', s) <- runParser (spanP isNotQuotation) remain
  (remain'', c) <- runParser (charP '"') remain'
  return (remain'', JsonString s)

-- runParser arraySplit :: String -> (String, [JsonValue])
-- "[123,456,789]"
arraySplit :: Parser [JsonValue]
arraySplit = Parser $ \input ->
  case runJP input of
    Just (']' : xs, j) -> Just (']' : xs, [j])
    Just (',' : xs, j) ->
      case runAP xs of
        Just (x, js) -> Just (x, j : js)
        _ -> Nothing
    _ -> Nothing
  where
    runAP = runParser arraySplit
    runJP = runParser jsonValue
    commmaP = charP ','

jsonArray :: Parser JsonValue
jsonArray = Parser $ \input -> do
  (remain, c) <- runParser (charP '[') input
  (remain', s) <- runParser arraySplit remain
  (remain'', c) <- runParser (charP ']') remain'
  return (remain'', JsonArray s)


-- runParser objectSplit :: String -> (String, [(String, JsonValue)])
-- "{id:1,num:2}"
objectSplit :: Parser [(String, JsonValue)]
objectSplit = Parser $ \input ->
  case runJP input of
    Just ('}' : xs, j) -> Just ('}' : xs, [j])
    Just (',' : xs, j) ->
      case runOP xs of
        Just (x, js) -> Just (x, j : js)
        _ -> Nothing
    _ -> Nothing
  where
    runOP = runParser objectSplit
    runJP = runParser objectValue
    commmaP = charP ','

objectValue :: Parser (String, JsonValue)
objectValue = Parser $ \input -> 
    case runParser keyP input of
        Just (x, key) -> do
            (x', c) <- runParser (charP ':') x
            (x'', value) <- runParser jsonValue x'
            return (x'', (key, value))
        _ -> Nothing
    where
        keyP = spanP isNotColon


jsonObject :: Parser JsonValue
jsonObject = Parser $ \input -> do
  (remain, c) <- runParser (charP '{') input
  (remain', s) <- runParser objectSplit remain
  (remain'', c) <- runParser (charP '}') remain'
  return (remain'', JsonObject s)

main :: IO ()
main = undefined