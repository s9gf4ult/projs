{-# LANGUAGE Arrows #-}

import FRP.Yampa
import Data.IORef
import Data.Time.Clock
import Control.Arrow
import Control.Monad
import Control.Concurrent
 
sf :: SF () (IO Bool) -- The signal function to be run
sf = proc inp -> do
  quit <- (now ()) >>> (delayEvent 5) -< ()
  t <- integral -< (2 :: Double)
  returnA -< do
    putStrLn $ show t
    return $ isEvent quit

sense timeRef _ = do
  threadDelay 200000
  putStrLn "sense ..."
  t' <- getCurrentTime
  t <- readIORef timeRef
  let dt = realToFrac (diffUTCTime t' t) -- Time difference in seconds
  writeIORef timeRef t'
  return (dt, Nothing) -- we could equally well return (dt, Just ())

actuate _ x = do
  exit <- x
  when exit $ putStrLn "exit ..."
  return exit
  
main :: IO ()
main = do
  t <- getCurrentTime -- Current UTC time
  timeRef <- newIORef t
  reactimate (putStrLn "Starting ...") (sense timeRef) actuate sf
