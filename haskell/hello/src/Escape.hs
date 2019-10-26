module Escape where

import           Control.Monad.Base
-- import           Control.Monad.Catch
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Control

escTest :: IO ()
escTest = evalContT $ do
  callCC $ \esc -> do
    let
      ma = do
        liftBase $ print "action"
        esc ()
    finally ma $
      liftBase $ print "finally"
  liftBase $ print "finish"
