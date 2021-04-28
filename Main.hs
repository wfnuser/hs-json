module Main where

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

data JsonValue = JsonNull 
    | JsonBool Bool 
    deriving (Show, Eq)

jsonValue :: Parser JsonValue
jsonValue = undefined


main :: IO ()
main = undefined