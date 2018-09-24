-- | Kinda external service

module Lib.Service where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import Data.IORef
import GHC.Generics (Generic)

data User = Ivan | Anon

data UserService m = UserService
  { currentUser :: m User
  , login       :: m ()
  , logout      :: m ()
  } deriving (Generic)

newUserService :: MonadBase IO m => IORef User -> UserService m
newUserService u =
  let
    currentUser = liftBase $ do
      putStrLn "Geting user from database"
      readIORef u
    login = liftBase $ do
      putStrLn "Login"
      writeIORef u Ivan
    logout = liftBase $ do
      putStrLn "Logout"
      writeIORef u Anon
  in UserService{..}

-- Note that to construct fake service we need weaken constraints
constUserService :: Applicative m => User -> UserService m
constUserService u = UserService
  { currentUser = pure u
  , login = pure ()
  , logout = pure ()
  }
