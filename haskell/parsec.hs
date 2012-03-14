module Main where
import Text.ParserCombinators.Parsec


numbers :: Parser String
numbers = many1 (oneOf "0123456789")

floats :: Parser (String, String, String)
floats = do
  a <- numbers
  b <- do { char '.'
          ; b <- numbers <|> return []
          ; return b } <|> return []
  c <- do { oneOf "eEеЕЁё"
          ; x <- numbers
          ; return x} <|> return []
  return (a, b, c)

-- readfloat :: (Fractional a , Read a) => String -> Maybe a
-- readfloat st = alpha $ parse floats "the name" st
--                where alpha (Left _) = Nothing
--                      alpha (Right (a, b))  = Just (black a + blue b)
--                      black a = foldr (+) 0 $ fmap mul $ zip (reverse a) [10^x | x <- [0..]]
--                      blue b = foldr (+) 0 $ fmap mul $ zip b [1 / (10^x) | x <- [1..]]
--                      mul (x, y) = (read [x]) * y