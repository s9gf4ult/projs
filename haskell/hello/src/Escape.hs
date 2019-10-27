module Escape where

import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Cont
import           Control.Monad.Trans.Cont (evalContT)
import           Data.Foldable
import           Data.IORef

escTest :: Integer -> [Integer] -> IO [Integer]
escTest lim as = evalContT $ do
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

-- | Should not compile
isolateTest :: ControlT IO s ()
isolateTest = do
  callCC $ \exit -> do
    isolate $ do
      return ()
    exit ()

newtype ControlT (m :: * -> *) (s :: k) (a :: *) = ControlT
  { unControlT :: ContT a m a
  }

instance Functor (ControlT m s)
instance Applicative (ControlT m s)
instance Monad (ControlT m s)
instance MonadCont (ControlT m s)
instance MonadThrow (ControlT m s)


-- class (forall s. MonadThrow (m s)) => MultiMonadCatch (m :: k -> * -> *) where
--   multiCatch
--     :: forall s e a
--     .  (Exception e)
--     => (forall t. m t a)
--     -> (e -> m s a)
--     -> m s a

-- instance MultiMonadCatch (ControlT m)

-- class MultiMonadMask (m :: k -> * -> *) where
--   multiMask

contCatch
  :: (MonadCatch m, Exception e)
  => (forall s. ControlT m s a)
  -> (forall q. e -> ControlT m q a)
  -> ControlT m t a
contCatch ma handler = ControlT $ lift $ catch (evalControlT ma) (evalControlT . handler)

contMask
  :: forall t m b
  .  (MonadMask m)
  => (forall s. (forall a. m a -> m a) -> ControlT m s b)
  -> ControlT m t b
contMask ma = controlLift $ mask $ \restore ->
  evalControlT (ma restore)

evalControlT :: (Monad m) => ControlT m s a -> m a
evalControlT (ControlT ma) = evalContT ma

controlLift :: (Monad m) => m a -> ControlT m s a
controlLift = ControlT . lift

isolate :: (Monad m) => (forall s. ControlT m s a) -> ControlT m t a
isolate ma = ControlT $ lift $ evalControlT ma
