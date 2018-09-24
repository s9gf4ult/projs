module Lib.RecReaderT where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import Data.Kind
import GHC.Generics (Generic)
import GHC.TypeLits

newtype RecReaderT s m a = RecReaderT (ReaderT (s (RecReaderT s m)) m a)
  deriving (Functor, Applicative, Monad)

deriving instance (Monad m, MonadBase b m) => MonadBase b (RecReaderT s m)

deriving instance (Monad m) => MonadReader (s (RecReaderT s m)) (RecReaderT s m)

runRecReaderT :: s (RecReaderT s m) -> RecReaderT s m a -> m a
runRecReaderT struct (RecReaderT r) = runReaderT r struct

type family HasFields s (fields :: [(Symbol, *)]) :: Constraint where
  HasFields s ( '(x, a) ': rest ) = (HasField' x s a, HasFields s rest)
  HasFields s '[]                 = ()
