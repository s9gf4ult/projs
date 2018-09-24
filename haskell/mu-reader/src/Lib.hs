module Lib where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import GHC.Generics (Generic)

data TruthService m = TruthService
  { truth :: m Int
  } deriving (Generic)

truthService :: (MonadBase IO m) => TruthService m
truthService = TruthService
  (liftBase (putStrLn "Calculating truth") *> return 42)

lieService :: (Monad m) => TruthService m
lieService = TruthService (return 666)

data MultipleService m = MultipleService
  { truth :: m Int
  , currentUser :: m Int
  } deriving (Generic)

multipleService :: (MonadBase IO m) => MultipleService m
multipleService = MultipleService
  { truth = liftBase (putStrLn "Haha !") *> return 55
  , currentUser = return 100
  }

useTruthService
  :: forall m r
  . (MonadBase IO m, MonadReader r m, HasField' "truth" r (m Int))
  => m ()
useTruthService = do
  liftBase $ putStrLn "Now the truth appears"
  s <- ask
  t <- s ^. field' @"truth"
  liftBase $ print t

newtype RecTransT s t m a = RecTransT (t (s (RecTransT s t m)) m a)

type ApplyStruct s t m = s (RecTransT s t m)

type ApplyTrans s t m = t (ApplyStruct s t m) m

deriving instance Functor (ApplyTrans s t m) => Functor (RecTransT s t m)

deriving instance Applicative (ApplyTrans s t m) => Applicative (RecTransT s t m)

deriving instance (Monad (ApplyTrans s t m), MonadBase b (ApplyTrans s t m)) => MonadBase b (RecTransT s t m)

-- deriving instance (Monad m) => MonadReader (s (RecTransT s m)) (RecTransT s m)

runRecReaderT :: s (RecTransT s ReaderT m) -> RecTransT s ReaderT m a -> m a
runRecReaderT struct (RecTransT r) = runReaderT r struct

test :: IO ()
test = do
  runRecTransT truthService useTruthService
  runRecTransT lieService useTruthService
  runRecTransT multipleService useTruthService
