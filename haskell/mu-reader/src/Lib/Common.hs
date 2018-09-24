module Lib.Common where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import GHC.Generics (Generic)
import Lib.RecReaderT
import Lib.Service

someServiceCode
  :: (MonadReader r m, HasLogger r m, HasUserService r m)
  => m ()
someServiceCode = do
  userService <- view $ field' @"userService"
  void $ do
    user <- currentUser userService
    logStr $ "Current user is " <> show user
  login userService
  void $ do
    user <- currentUser userService
    logStr $ "After login user is " <> show user
  logout userService
  void $ do
    user <- currentUser userService
    logStr $ "After logout user is " <> show user
