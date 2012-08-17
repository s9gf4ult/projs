{-# LANGUAGE Arrows #-}

import FRP.Yampa
import Data.IORef
import Data.Time.Clock
import Control.Arrow
import Control.Monad
import System.Posix
 
sf :: SF () (IO Bool) -- The signal function to be run
-- sf = time >>> arr (\t -> if (t < 10) then False else True)
-- the time signal function ignores its input and returns the time
sf = proc inp -> do
  quit <- (now ()) >>> (delayEvent 5) -< ()
  t <- time -< ()
  returnA -< do
    putStrLn $ show t
    return $ isEvent quit
  -- returnA -< (do {
  --                putStrLn $ show t;
  --                return $ isEvent quit
  --                })
  
 
main :: IO ()
main = do
  t <- getCurrentTime -- Current UTC time
  timeRef <- newIORef t
  let init = putStrLn "Starting ..."
      sense = (\_ -> do
                  w <- sleep 1
                  putStrLn "sense ..."
                  t' <- getCurrentTime
                  t <- readIORef timeRef
                  let dt = realToFrac (diffUTCTime t' t) -- Time difference in seconds
                  writeIORef timeRef t'
                  return $ w `seq` (dt, Nothing)) -- we could equally well return (dt, Just ())
      actuate = (\_ x -> do
                    exit <- x
                    when exit $ putStrLn "exit ..."
                    return exit)
                      
                           
  reactimate init sense actuate sf
