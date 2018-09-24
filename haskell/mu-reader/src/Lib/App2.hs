module Lib.App2 where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import GHC.Generics (Generic)
import Lib.Common
import Lib.Service

data App2 m = App2
  { userService :: UserService m
  , logger      :: String -> m ()
  , field1      :: Int
  , field2      :: Int
  , field3      :: Int
  , field4      :: Int
  , field5      :: Int
  , field6      :: Int
  , field7      :: Int
  , field8      :: Int
  , field9      :: Int
  , field10     :: Int
  , field11     :: Int
  , field12     :: Int
  , field13     :: Int
  , field14     :: Int
  , field15     :: Int
  } deriving (Generic)

newApp2 :: MonadBase IO m => UserService m -> App2 m
newApp2 us = App2
  { userService = us
  , logger = \a -> liftBase $ putStrLn $ "[App2] " <> a
  , field1 = 1
  , field2 = 2
  , field3 = 3
  , field4 = 4
  , field5 = 5
  , field6 = 6
  , field7 = 7
  , field8 = 8
  , field9 = 9
  , field10 = 10
  , field11 = 11
  , field12 = 12
  , field13 = 13
  , field14 = 14
  , field15 = 15
  }


app2Handler :: (MonadReader r m, HasLogger r m, HasUserService r m) => m ()
app2Handler = do
  logStr "==================== app2Handler"
  someServiceCode
  let revLogger logger s = logger $ reverse s -- lol!
  logStr "====================== reversed"
  local (over (field' @"logger") revLogger) $ do
    someServiceCode
