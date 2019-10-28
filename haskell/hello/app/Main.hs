module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Data.Coerce
import           Data.Foldable
import           Data.IORef
import           Escape
import           System.Environment


escTest :: Integer -> [Integer] -> IO [Integer]
escTest lim as = evalControlT $ do
  r <- liftBase $ newIORef []
  callCC $ \done -> do
    for_ as $ \a -> callCC $ \next -> if
      | a > 1 -> do
        primes <- liftBase $ readIORef r
        when (a > lim) (done primes)
        for_ primes $ \prime -> do
          when (a `mod` prime == 0) (next ())
        liftBase $ do
          writeIORef r (a : primes)
          print a
      | otherwise -> next ()
    liftBase $ readIORef r


put :: (MonadBase IO m) => String -> m ()
put = liftBase . putStrLn

type CR r m s a = a -> ControlT r m s a

stuffWrap :: (MonadBase IO m) => (CR r m s a -> CR r m s a -> ControlT r m s a) -> ControlT r m s a
stuffWrap ma = do
  callCC $ \exit -> do
    res <- callCC $ \rollback -> do
      res <- callCC $ \commit -> do
        res <- ma commit rollback
        put "Exited"
        exit res
      put "Commit called"
      exit res
    put "Rollback called"
    exit res

stuffTest :: IO ()
stuffTest = evalControlT $ do
  stuffWrap $ \com rol -> do
    put "Doing nothing"
  stuffWrap $ \com rol -> do
    put "Call com"
    com ()
  stuffWrap $ \com rol -> do
    put "Call roll"
    rol ()
  stuffWrap $ \com rol -> do
    put "Failing"
    contCatch (fail "oh shi")
      (\ (e :: SomeException) -> put "Error catched")
  -- stuffWrap $ \con rol -> do
  --   put "Not compiling"
  --   contCatch (fail "oh shi")
  --     (\ (e :: SomeException) -> do
  --         put "Error catched"
  --         coerce $ con ()
  --         )

main :: IO ()
main = do
  stuffTest
