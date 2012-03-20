module Main where

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class (lift)
import Data.Map as M
import System.Environment
import System.IO
import NGram

main = do
  a <- getArgs
  if length a /= 2
    then usage
    else do let len = read $ a !! 0
            cont <- readFile $ a !! 1
            let result = show ((makeNgram len cont) :: M.Map String Double)
            putStrLn result
              

usage :: IO ()
usage = putStrLn "Need 2 arguments :: length filename"