module Main where
import Text.ParserCombinators.Parsec


numbers :: Parser String
numbers = many1 (oneOf "0123456789")

floats :: Parser (String, String)
floats = do
  a <- numbers
  b <- do { char '.'
          ; b <- numbers <|> return []
          ; return b } <|> return []
  return (a, b)