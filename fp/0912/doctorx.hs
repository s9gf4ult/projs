
module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Trans.Error
import Control.Monad.Trans

data Person = Person {persNmb :: Integer,
                      persSymbol :: Char,
                      persOpinions :: [Opinion]}
              deriving (Show, Eq)
           
data Opinion = OpCrazy Integer
             | OpNoraml Integer
             deriving (Show, Eq)

parsePeople :: Parser [Person]
parsePeople = do
  res <- sepEndBy (try parseMan) (char '\n')
  eof
  return res
  
parseMan :: Parser Person
parseMan = do
  spaces
  nmb <- parseInt
  spaces
  char ':'
  spaces
  char '\''
  ch <- anyChar
  char '\''
  spaces
  ops <- option [] $ do 
    char ','
    spaces
    q <- sepBy parseOpinion $ try $ do
      spaces
      char ','
      spaces
    return q
  spaces
  return Person {persNmb = nmb,
                 persSymbol = ch,
                 persOpinions = ops}
  
parseInt :: Parser Integer
parseInt = do
  d <- many1 digit
  case (reads d) of
    [(a, "")] -> return a
    _         -> return 0 -- stupid

parseOpinion :: Parser Opinion
parseOpinion = do
  crazy <- option False $ do
    char '-'
    return True
  i <- parseInt
  return $ if crazy
           then OpCrazy i
           else OpNoraml i
  
  