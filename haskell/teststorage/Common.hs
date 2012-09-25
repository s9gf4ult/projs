module Common where

import Data.Int
import System.Random
import Control.DeepSeq
import Control.Monad.Trans.Error
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Time.Clock

data Storable = Storable Int32 Int32 Int64 String
              deriving (Eq, Show)

instance Random Storable where
  random g = (Storable a b c d, gn)
    where
      (a, g1) = randomR (0, 1000) g
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

data Condition = CLT Int32
               | CGT Int32
               | CLGT Int32 Int32
  
class Storage a where
  saveS :: (MonadIO m, Monad m) => a -> [Storable] -> ErrorT String m ()
  getS :: (Monad m, MonadIO m) => a -> ErrorT String m [Storable]
  getFilterA :: (MonadIO m, Monad m) => Condition -> a -> ErrorT String m [Storable]
  resetS :: a -> (Monad m, MonadIO m) => ErrorT String m ()

measureTime :: (Monad m, MonadIO m, NFData a) => (a -> String) -> ErrorT String m a -> ErrorT String m a
measureTime s m = do
  t1 <- liftIO getCurrentTime
  res <- t1 `deepseq` m
  t2 <- res `deepseq` liftIO getCurrentTime
  liftIO $ putStrLn $ (s res) ++ " took " ++ (show $ diffUTCTime t2 t1) ++ " seconds"
  return res

littleTest :: (Storage a, Monad m, MonadIO m) => Int -> a -> ErrorT String m ()
littleTest inscount a = do
  resetS a
  measureTime (\_ -> "Inserting " ++ (show inscount) ++ " elements") $ do
    x <- liftIO genStorables
    saveS a $ take inscount x
  measureTime (\x -> "Fetching " ++ (show $ length x) ++ " elemtns") $ getS a
  return ()
  
mapLeftE :: (Monad m) => (a -> b) -> ErrorT a m c -> ErrorT b m c
mapLeftE f m = ErrorT $ runErrorT m >>= return . mapl
    where
      mapl (Left x) = Left $ f x
      mapl (Right x) = Right x

maybeToErrorT :: (Monad m, Error b) => b -> Maybe a -> ErrorT b m a
maybeToErrorT _ (Just x) = return x
maybeToErrorT b Nothing = throwError b