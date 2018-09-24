-- | Kinda external service

module Lib.Service where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import Data.IORef
import GHC.Generics (Generic)

data User = Ivan | Anon
  deriving (Eq, Ord, Show)

data UserService m = UserService
  { currentUser :: m User
  , login       :: m ()
  , logout      :: m ()
  } deriving (Generic)

type HasLogger r m = HasField' "logger" r (String -> m ())

logStr
  :: (MonadReader r m, HasLogger r m)
  => String
  -> m ()
logStr s = do
  f <- view $ field' @"logger"
  f s

type HasUserService r m = HasField' "userService" r (UserService m)

newUserService
  :: (MonadBase IO m, MonadReader r m, HasLogger r m)
  => IORef User
  -> UserService m
newUserService u =
  let
    currentUser = do
      logStr "Geting user from database"
      liftBase $ readIORef u
    login = do
      logStr "Login"
      liftBase $ writeIORef u Ivan
    logout = do
      logStr "Logout"
      liftBase $ writeIORef u Anon
  in UserService{..}

-- Note that to construct fake service we need weaken constraints
constUserService :: Applicative m => User -> UserService m
constUserService u = UserService
  { currentUser = pure u
  , login = pure ()
  , logout = pure ()
  }
