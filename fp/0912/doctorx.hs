
module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Trans.Error
import Control.Monad.Trans

data Person = Person {persNmb :: Int,
                      persSymbol :: Char,
                      persOpinions :: [Opinion Int]}
              deriving (Show, Eq)
           
data Opinion a = Crazy a
               | Noraml a 
               deriving (Show, Eq)

parsePeople :: Parser [Person]
parsePeople = do
  many newline
  res <- sepEndBy parseMan newline
  many newline
  eof
  return res

spcs :: Parser ()
spcs = do
  many $ choice [char ' ',
                 char '\t']
  return ()
  
parseMan :: Parser Person
parseMan = do
  spcs
  nmb <- parseInt
  spcs
  char ':'
  spcs
  char '\''
  ch <- anyChar
  char '\''
  spcs
  ops <- option [] $ do 
    char ','
    spcs
    q <- sepBy parseOpinion $ try $ do
      spcs
      char ','
      spcs
    return q
  spcs
  return Person {persNmb = nmb,
                 persSymbol = ch,
                 persOpinions = ops}
  
parseInt :: Parser Int
parseInt = do
  d <- many1 digit
  case (reads d) of
    [(a, "")] -> return a
    _         -> return 0 -- stupid

parseOpinion :: Parser (Opinion Int)
parseOpinion = do
  crazy <- option False $ do
    char '-'
    return True
  i <- parseInt
  return $ if crazy
           then Crazy i
           else Noraml i
  
  