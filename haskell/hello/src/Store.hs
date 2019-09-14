module Store where

import           Control.Comonad
import           Control.Monad
import           Data.Set        (Set)
import qualified Data.Set        as S

data Store s a = Store s (s -> a)
  deriving (Functor)

instance Comonad (Store s) where
  extract (Store s f) = f s
  duplicate (Store s sa) = Store s $ \q -> Store q sa

type Cell = (Int, Int)

setState :: s -> Store s a -> Store s a
setState s (Store _ f) = Store s f

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment f (Store s fa) = fa <$> f s

newGame :: Set Cell -> Store Cell Bool
newGame cells = Store (0, 0) $ \cell -> S.member cell cells

neighbours :: Cell -> [Cell]
neighbours cell@(a, b) = do
  aa <- [a-1, a, a+1]
  bb <- [b-1, b, b+1]
  let newCell = (aa, bb)
  guard $ newCell /= cell
  return newCell

step :: Store Cell Bool -> Store Cell Bool
step = extend $ \st ->
  let ns = experiment neighbours st
      nc = length $ filter id ns
      live = case extract st of
        True
          | nc < 2             -> False
          | nc == 2 || nc == 3 -> True
          | otherwise          -> False
        False
          | nc == 3   -> True
          | otherwise -> False
  in live
