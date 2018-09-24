module Lib.App1 where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import GHC.Generics (Generic)
import Lib.Service

data App1 m = App1
  { userService :: UserService m
  , logger      :: String -> m ()
  } deriving (Generic)

newApp :: (MonadBase IO m) => UserService m -> App1 m
newApp us = App1
  { userService = us
  , logger = \a -> liftBase $ putStrLn $ "[App1]" <> a  }
