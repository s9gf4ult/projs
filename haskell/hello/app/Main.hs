module Main where

import           Control.Monad
import           Escape
import           System.Environment

main :: IO ()
main = do
  [a] <- getArgs
  void $ escTest (read a) [1..]
