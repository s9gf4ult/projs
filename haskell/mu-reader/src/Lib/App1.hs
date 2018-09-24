module Lib.App1 where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import GHC.Generics (Generic)
import Lib.Common
import Lib.Service

data App1 m = App1
  { userService :: UserService m
  , logger      :: String -> m ()
  } deriving (Generic)

newApp1 :: (MonadBase IO m) => UserService m -> App1 m
newApp1 us = App1
  { userService = us
  , logger = \a -> liftBase $ putStrLn $ "[App1] " <> a  }

app1Handler
  :: (MonadReader r m, HasLogger r m, HasUserService r m)
  => m ()
app1Handler = do
  logStr "==================== app1Handler "
  someServiceCode
