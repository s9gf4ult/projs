{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, BangPatterns #-}
module NGram where

import System.Random
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.Map as M
import Control.Arrow
import Data.List (inits, tails)
import System.Environment

getRandomElement :: (Random a, Num a, Ord a) => [(a, b)] -> MaybeT IO b
getRandomElement [] = MaybeT $ return Nothing
getRandomElement xs = MaybeT $ do
  el <- randomRIO (0, probSum xs)
  return $ Just $ snd $ head $ filter (\((l, g), ret) -> el >= l && el <= g) lowHighList
  where
    -- probSum :: (Ord a, Num a) => [(a, b)] -> a
    probSum = sum . map fst
    -- lowHighList :: (Random a, Num a, Ord a) => [((a, a), b)]
    lowHighList = map (((\x -> (probSum x) - (last $ map fst x)) &&& probSum) &&& (last . map snd)) $
                   tail $ inits xs


makeNgram :: (Ord a, Num b) => Int -> [a] -> M.Map [a] b
makeNgram n str = foldl foldf M.empty $ nlists n str
  where
    foldf m s = M.insertWith (+) s 1 m
    nlists :: Int -> [a] -> [[a]]
    nlists n str = map (take n)
                   $ take (length str + 1 - n)
                   $ tails str

generateSeq :: (Random b, Ord b, Num b) => Int -> M.Map [a] b -> MaybeT IO [a]
generateSeq amount ng = (mapM id $
                          replicate amount $
                          getRandomElement $
                          map (snd &&& fst) $
                          M.toList ng) >>= return . concat

