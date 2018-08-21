module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [a] <- getArgs
  print
    $ lazyUniq
    $ runGen (SeqLen $ read a)
    $ allBraces [roundBrace, squareBrce]
