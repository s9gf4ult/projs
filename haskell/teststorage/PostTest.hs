module PostTest where

import Common
import PostInstance
import Database.PostgreSQL.Simple
import Control.Monad.Trans.Error
import System.Environment
import Safe
import Control.Monad.IO.Class

main = do
  ret <- runErrorT $ do
    a <- liftIO getArgs
    c <- execLeft $ connect $ ConnectInfo "127.0.0.1" 5432 "test" "test" "test"
    if (length a == 1)
      then do
        aa <- maybeToErrorT "pleas insert the number of things to thest with" $ readMay $ head a
        littleTest aa c
      else littleTest 10000 c
  case ret of
    Left e -> putStrLn e
    Right _ -> return ()
  