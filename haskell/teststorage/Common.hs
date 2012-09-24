module Common where

import Data.Int
import System.Random
import Control.DeepSeq
import Control.Monad.Trans.Error
import Control.Monad.IO.Class

data Storable = Storable Int32 Int32 Int64 String

instance Random Storable where
  random g = (Storable a b c d, gn)
    where
      (a, g1) = random g
      (b, g2) = random g1
      (c, gn) = random g2
      d = take 10 $ randomRs ('a', 'z') gn
  randomR _ g = random g        --  FIXME: we just dont need it now

instance NFData Storable where
  rnf (Storable a b c d) = a `deepseq` b `deepseq` c `deepseq` d `deepseq` ()

genStorables :: IO [Storable]
genStorables = do
  g <- newStdGen
  return $ randoms g

data Condition = LT Int32
               | GT Int32
               | LGT Int32 Int32
  
class Storage a where
  saveS :: (MonadIO m, Monad m) => a -> [Storable] -> ErrorT String m ()
  getS :: (Monad m, MonadIO m) => a -> ErrorT String m [Storable]
  getFilterA :: (MonadIO m, Monad m) => Condition -> a -> ErrorT String m [Storable]