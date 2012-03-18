{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, BangPatterns #-}
module NGram where

import System.Random
import System.Environment
import System.IO
import Control.Arrow ((&&&))
import Control.Monad
import Data.List (inits, tails)
import Data.Text (pack, unpack, replace)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.HashTable as T
import Data.Monoid

getRandomElement :: (Random a, Num a, Ord a) => [(a, b)] -> IO (Maybe b)
getRandomElement [] = return Nothing
getRandomElement xs = do
  el <- randomRIO (0, probSum xs)
  return $ Just $ snd $ head $ filter (\((l, g), ret) -> el >= l && el <= g) lowHighList
  where
    -- probSum :: (Ord a, Num a) => [(a, b)] -> a
    probSum = sum . map fst

    -- lowHighList :: (Random a, Num a, Ord a) => [((a, a), b)]
    lowHighList = map (((\x -> (probSum x) - (last $ map fst x)) &&& probSum) &&& (last . map snd)) $
                   tail $ inits xs


nlists :: Int -> [a] -> [[a]]
nlists n str = map (take n)
               $ take (length str + 1 - n)
               $ tails str

makeNgram :: (Ord a,Num b) => Int -> [a] -> M.Map [a] b
makeNgram n str = foldr foldf M.empty $ nlists n str
  where
    foldf s m = M.insertWith (+) s 1 m

makeHNgram :: (Num a) => Int -> String -> IO (T.HashTable String a)
makeHNgram n str = foldr foldf (T.new (==) T.hashString) $ nlists n str
  where
    foldf st hio = do h <- hio
                      found <- T.lookup h st
                      if found == Nothing
                        then T.insert h st 1
                        else do T.delete h st
                                T.insert h st (1 + (fromMaybe 0 found))
                      return h
                      
ruleFromList :: (AsAtom a) => [(a, Int)] -> Rule
ruleFromList m = foldr (<|>) (Rule []) $ map (\(a, b) -> b <:> a) m

replicateRule :: Int -> Rule -> [Atom]
replicateRule n r = foldr (<+>) [] $ replicate n r

generateText :: Int -> Int -> String -> IO String
generateText len amount s = flattenRule (rule $ replicateRule amount $ ruleFromList $ M.toList $ makeNgram len s)

generateHText :: Int -> Int -> String -> IO String
generateHText len amount str = do ngram <- makeHNgram len str
                                  rulelist <- T.toList ngram
                                  flattenRule (rule $ replicateRule amount $ ruleFromList $ rulelist)



main = do
  a <- getArgs
  if length a /= 3
    then usage
    else do let len = read $ a !! 0
            let amount = read $ a !! 1
            cont <- readFile $ a !! 2
            result <- generateHText len amount cont
            putStrLn result
              

usage :: IO ()
usage = putStrLn "Need 3 arguments :: length amount filename"