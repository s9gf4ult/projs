module Lib where

import Control.Lens
import Control.Monad.Base
import Control.Monad.Reader
import Data.Generics.Product
import Data.IORef
import Lib.App1
import Lib.App2
import Lib.RecReaderT
import Lib.Service

import GHC.Generics (Generic)

test :: IO ()
test = do
  app1 <- do
    userRef <- newIORef Anon
    let us = newUserService userRef
    return $ newApp1 us
  runRecReaderT app1 app1Handler
  runRecReaderT app1 app2Handler
  app2 <- do
    userRef <- newIORef Anon
    let us = newUserService userRef
    return $ newApp2 us
  runRecReaderT app2 app1Handler
  runRecReaderT app2 app2Handler
