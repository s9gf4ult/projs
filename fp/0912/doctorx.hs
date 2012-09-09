
module Main where

import Text.Parsec
import Text.Parsec.String
import Control.Monad.Trans.Error
import Control.Monad.Trans
import System.Environment
import Data.List (sort)
import Data.Maybe (catMaybes)
import Data.Set (empty, toList, difference, fromList, insert)

data Person = Person {persNmb :: Int,
                      persSymbol :: Char,
                      persOpinions :: [Opinion Int]}
              deriving (Show, Eq, Ord)
           
data Opinion a = Crazy a
               | Noraml a
               deriving (Show, Eq, Ord)

data PersonType a = PNormal a
                  | PCrazy a
                  | PDoctorX a
                  deriving (Show, Eq, Ord)

isDoctor (PDoctorX a) = True
isDoctor _ = False

parsePeople :: Parser [Person]
parsePeople = do
  many newline
  res <- sepEndBy parseMan (many1 newline)
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

solve :: [Person] -> [[PersonType Person]]
solve [] = []
solve (x:xs) = catMaybes $ (solve_ empty (PNormal x) xs) ++
               (solve_ empty (PCrazy x) xs) ++ (solve_ empty (PDoctorX x) xs)
  where
    solve_ known current [] = [Just $ current:(toList known)]
    solve_ known current unknown | notPosible (insert current known) unknown = [Nothing]
                                 | 

    notPosible = undefined

                            

printError :: (Monad m) => m (Either ParseError a) -> m (Either String a)
printError p = do
  a <- p
  case a of
    Left e -> return $ Left $ show e
    Right val -> return $ Right $ val

printSolutions :: [[PersonType Person]] -> Int -> IO ()
printSolutions sol col = mapM_ printS $ zip sol [1..]
  where
    printS :: ([PersonType Person], Int) -> IO ()
    printS (s, c) = do
      putStrLn $ "Solution " ++ (show c) ++ ":"
      case (filter isDoctor s) of
        [(PDoctorX d)] -> do
          putStrLn $ "DoctorX is " ++ (show $ persNmb d)
          printMatrix col $ filter (not . isDoctor) s
        _ -> putStrLn "is wrong ... no one or more then one doctors found"

printMatrix c a = mapM_ putStrLn $ makePicture c a

makePicture :: Int -> [PersonType Person] -> [String]
makePicture c s = splits $ normals ++ crazys
  where
    normals = sort $ map persSymbol $ getNormal s
    crazys = reverse $ sort $ map persSymbol $ getCrazy s
    getNormal [] = []
    getNormal ((PNormal a):xs) = a:(getNormal xs)
    getNormal (_:xs) = getNormal xs
    getCrazy [] = []
    getCrazy ((PCrazy a):xs) = a:(getCrazy xs)
    getCrazy (_:xs) = getCrazy xs
    splits [] = []
    splits x = shead:(splits stail)
      where
        (shead, stail) = splitAt c x

readVal :: (Read a, Monad m) => String -> ErrorT String m a
readVal s = case (reads s) of
  [(a, "")] -> return a
  _         -> throwError $ "could not convert string \"" ++ s ++ "\" to value"

main = do
  ret <- runErrorT $ do
    args <- lift getArgs
    case args of
      [fname, cols] -> do
        columns <- readVal cols
        people <- ErrorT $ printError $ parseFromFile parsePeople fname
        let s = solve people
        lift $ printSolutions s columns
  
      _ -> throwError "Need 2 arguments: filename and count of columns to print image"
  
  case ret of
    Left e -> putStrLn e
    Right x -> return ()