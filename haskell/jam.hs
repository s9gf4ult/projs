module Main where

import System.Environment
import Data.Numbers.Primes
import System.IO

ert [] = []
ert (x:xs) = x:(ert others)
  where
    others = filter (\a -> a `mod` x /= 0) xs


    

readall :: (Read a) => String -> [a]
readall s = readall' [] s
  where
    readall' ac "" = ac
    readall' ac st = case reads st of
      []        -> readall' ac $ tail st
      [(x, tl)] -> readall' (x:ac) tl


main = do
  args <- getArgs
  if length args > 0
    then do
    a <- readFile $ args !! 0
    putStrLn $ show $ reverse (readall a :: [Int])
    else putStrLn "need Argument"
    