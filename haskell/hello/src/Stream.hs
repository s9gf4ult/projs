module Stream where

import           Control.Comonad
import           Control.Monad
import qualified Data.List       as L
import           GHC.Natural
import           Prelude         hiding (drop, sum, take)

data Stream a = Stream a (Stream a)
  deriving Functor

instance Applicative Stream where
  (Stream f fs) <*> (Stream a as) = Stream (f a) (fs <*> as)

instance Comonad Stream where
  extract (Stream a _) = a
  duplicate x@(Stream _ s) = Stream x (duplicate s)

take :: Natural -> Stream a -> [a]
take 0 _            = []
take n (Stream a s) = a:take (pred n) s

drop :: Natural -> Stream a -> Stream a
drop 0 a            = a
drop n (Stream _ s) = drop (pred n) s

-- | Generates stream of sums of n elements
sum :: Num n => Natural -> Stream n -> Stream n
sum n = extend $ \s -> L.sum $ take n s

-- | How to implement this in comonads?
fastSum :: Num n => Natural -> Stream n -> Stream n
fastSum n x = Stream init (go init x $ drop n x)
  where
    init = L.sum $ take n x
    go prevS (Stream a as) (Stream t ts) =
      let newS = prevS - a + t
      in Stream newS (go newS as ts)

increasing :: Stream Natural
increasing = go 0
  where
    go n = Stream n (go $ succ n)
