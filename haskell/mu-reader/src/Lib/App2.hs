module Lib.App2 where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import GHC.Generics (Generic)
import Lib.Service

data App2 m = App2
  { userService   :: UserService m
  , logger        :: String -> m ()
  , somethingElse :: ()
  } deriving (Generic)
