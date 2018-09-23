module Lib where

import Control.Monad.Base
import Control.Monad.Reader

newtype Mu f = Mu (f (Mu f))


data Struct m = Struct
  { theTruth :: m Int
  }

struct :: (MonadBase IO m) => Struct m
struct = Struct
  (liftBase (print 43) *> return 42)

useStruct :: (MonadBase IO m, MonadReader (Struct m) m) => m Int
useStruct = do
  liftBase $ print "Not the trueth appears"
  s <- ask
  theTruth s


newtype SelfReader a m = SelfReader (ReaderT (Struct m) IO a)

type MuReader a = Mu (SelfReader a)

runStructReader :: Struct m -> ReaderT (Struct m) a -> IO a
runStructReader = error "FIXME: runStructReader not implemented"
