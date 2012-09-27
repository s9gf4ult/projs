module GenerateTest where

import Common
import Safe
import System.Environment
import Control.Monad.Trans.Error
import Control.Monad.IO.Class

  
main = do
  args <- getArgs
  res <- runErrorT $ do
    case args of
      [a] -> do
        ar <- maybeToErrorT "give me a number of instances to generate" $ readMay a
        work ar
      _ -> work 10000
  case res of
    Left e -> putStrLn e
    Right _ -> return ()

work :: Int -> (Monad m, MonadIO m) => ErrorT String m [Storable]
work c = measureTime wp $ liftIO $ do
  s <- genStorables
  return $ take c s
    where
      wp x = "generating " ++ (show $ length x) ++ " storables"