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
  --         con ()
  --         )


newtype ControlT (r :: *) (m :: * -> *) (s :: k) (a :: *) = ControlT
  { unControlT :: ContT r m a
  } deriving (Functor, Applicative, Monad, MonadCont, MonadThrow)

deriving instance (MonadBase b m) => MonadBase b (ControlT r m s)


-- class (forall s. MonadThrow (m s)) => IndexedMonadCatch (m :: k -> * -> *) where
--   indCatch
--     :: forall s e a
--     .  (Exception e)
--     => (forall t. m t a)
--     -> (forall q. e -> m q a)
--     -> m s a

-- instance (MonadCatch m) => IndexedMonadCatch (ControlT r m) where
--   indCatch = contCatch

-- class IndexesMonadMask (m :: k -> * -> *) where
--   indMask
--     :: forall s a
--     .  (forall t. (forall q b. m q b -> m t b) -> m t a)
--     -> m s a

-- instance (MonadMask m) => IndexesMonadMask (ControlT r m) where
--   indMask = contMask

contCatch
  :: (MonadCatch m, Exception e)
  => (forall s. ControlT a m s a)
  -> (forall q. e -> ControlT a m q a)
  -> ControlT r m t a
contCatch ma handler = liftControl
  $ catch (evalControlT ma) (evalControlT . handler)

contMask
  :: forall r m t b
  .  (MonadMask m)
  => (forall s. (forall a q . ControlT a m q a -> ControlT b m s a) -> ControlT b m s b)
  -> ControlT r m t b
contMask ma = liftControl $ mask $ \restore ->
  evalControlT (ma $ liftControl . restore . evalControlT)

evalControlT :: (Monad m) => ControlT a m s a -> m a
evalControlT (ControlT ma) = evalContT ma

runControlT :: (Monad m) => (a -> m r) -> ControlT r m s a -> m r
runControlT run (ControlT ma) = runContT ma run

liftControl :: (Monad m) => m a -> ControlT r m s a
liftControl = ControlT . lift

isolate :: (Monad m) => (forall s. ControlT a m s a) -> ControlT r m t a
isolate ma = ControlT $ lift $ evalControlT ma
