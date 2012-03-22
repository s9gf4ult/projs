module NGram where

import System.Random
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.Map as M
import Control.Arrow
import Data.List (inits, tails, foldl')
import System.Environment
import qualified Data.Vector as V

getRandomElement :: (Num a, Ord a) => a -> [((a, a), b)] -> Maybe b
getRandomElement _ [] = Nothing
getRandomElement elem xs = let el = max 0 $ min elem $ probSum xs
                           in Just $ snd $ head $ filter (\((l, g), ret) -> el >= l && el <= g) xs
    where
    probSum = snd . fst . last 

lowHighList :: Num a => [(a, b)] -> [((a, a), b)]
lowHighList xs = zip (zip sums $ tail sums) (map snd xs)
  where
    sums = scanl (+) 0 (map fst xs)
    

makeNgram :: (Ord a, Num b) => Int -> [a] -> M.Map [a] b
makeNgram n str = foldl' foldf M.empty $ nlists n str
  where
    foldf m s = M.insertWith (+) s 1 m
    nlists :: Int -> [a] -> [[a]]
    nlists n str = map (take n)
                   $ take (length str + 1 - n)
                   $ tails str

mutateNgram :: Ord b => M.Map a b -> M.Map b (V.Vector a)
mutateNgram ng = foldl foldf M.empty mlist
  where
    mlist = M.toList ng
    foldf mp (a, b) = M.insertWith (V.++) b (V.singleton a) mp

generateSeq :: (Random b, Ord b, Num b) => Int -> M.Map [a] b -> MaybeT IO [a]
generateSeq amount ng = do
  let mlist = lowHighList $ M.toList $ mutateNgram ng
  let msum  = (snd . fst . last) mlist
  gen <- lift newStdGen
  let rnds = take amount $ randomRs (0, msum) gen
  a <- MaybeT . return $ mapM (\x -> getRandomElement x mlist) rnds
  
  
  -- let mlist = lowHighList $ map (snd &&& fst) $ M.toList ng
  -- let fsum = (snd . fst . last) mlist
  -- gen <- lift newStdGen
  -- let rnds = take amount $ randomRs (0, fsum) gen
  -- MaybeT . return $ (mapM (\x -> getRandomElement x mlist) rnds) >>= return . concat
