import FRP.Yampa



import Data.IORef
import Data.Time.Clock
 
sf :: SF () Bool -- The signal function to be run
sf = time >>> arr (\t -> if (t < 10) then False else True)
-- the time signal function ignores its input and returns the time
 
main :: IO ()
main = do
  t <- getCurrentTime -- Current UTC time
  timeRef <- newIORef t
  let init = putStrLn "Hello... wait for it..."
      sense = (\_ -> do
                 t' <- getCurrentTime
                 t <- readIORef timeRef
                 let dt = realToFrac (diffUTCTime t' t) -- Time difference in seconds
                 writeIORef timeRef t'
                 return (dt, Nothing)) -- we could equally well return (dt, Just ())
      actuate = (\_ x -> if x
                          then putStrLn "World!" >> return x
                          else return x)
  reactimate init sense actuate sf
