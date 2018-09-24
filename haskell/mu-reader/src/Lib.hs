module Lib where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import GHC.Generics (Generic)

-- data TruthService m = TruthService
--   { truth :: m Int
--   } deriving (Generic)

-- truthService :: (MonadBase IO m) => TruthService m
-- truthService = TruthService
--   (liftBase (putStrLn "Calculating truth") *> return 42)

-- lieService :: (Monad m) => TruthService m
-- lieService = TruthService (return 666)

-- data MultipleService m = MultipleService
--   { truth :: m Int
--   , currentUser :: m Int
--   } deriving (Generic)

-- multipleService :: (MonadBase IO m) => MultipleService m
-- multipleService = MultipleService
--   { truth = liftBase (putStrLn "Haha !") *> return 55
--   , currentUser = return 100
--   }

-- useTruthService
--   :: forall m r
--   . (MonadBase IO m, MonadReader r m, HasField' "truth" r (m Int))
--   => m ()
-- useTruthService = do
--   liftBase $ putStrLn "Now the truth appears"
--   s <- ask
--   t <- s ^. field' @"truth"
--   liftBase $ print t

-- test :: IO ()
-- test = do
--   runRecReaderT truthService useTruthService
--   runRecReaderT lieService useTruthService
--   runRecReaderT multipleService useTruthService
