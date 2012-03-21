{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, BangPatterns #-}
module NGram where

import System.Random
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.Map as M
import Control.Arrow
import Data.List (inits, tails, foldl')
import System.Environment

getRandomElement :: (Num a, Ord a) => a -> [(a, b)] -> Maybe b
getRandomElement _ [] = Nothing
getRandomElement elem xs = let el = min elem $ probSum xs
                           in Just $ snd $ head $ filter (\((l, g), ret) -> el >= l && el <= g) lowHighList
    where
    -- probSum :: (Ord a, Num a) => [(a, b)] -> a
    probSum = sum . map fst
    -- lowHighList :: (Random a, Num a, Ord a) => [((a, a), b)]
    lowHighList = map (((\x -> (probSum x) - (last $ map fst x)) &&& probSum) &&& (last . map snd)) $
                   tail $ inits xs

makeNgram :: (Ord a, Num b) => Int -> [a] -> M.Map [a] b
makeNgram n str = foldl' foldf M.empty $ nlists n str
  where
    foldf m s = M.insertWith (+) s 1 m
    nlists :: Int -> [a] -> [[a]]
    nlists n str = map (take n)
                   $ take (length str + 1 - n)
                   $ tails str

generateSeq :: (Random b, Ord b, Num b) => Int -> M.Map [a] b -> MaybeT IO [a]
generateSeq amount ng = do
  let mlist = map (snd &&& fst) $ M.toList ng
  let fsum = sum $ map fst mlist
  gen <- lift newStdGen
  let rnds = take amount $ randomRs (0, fsum) gen
  MaybeT . return $ (mapM (\x -> getRandomElement x mlist) rnds) >>= return . concat
