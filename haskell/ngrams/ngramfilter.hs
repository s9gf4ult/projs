module Main where

import System.Environment
import System.IO
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.DeepSeq
import Data.Map as M
import NGram
  

main = runMaybeT $ do
  args <- lift $ getArgs
  if length args /= 3
    then lift usage
    else do let len = read $ args !! 0
            let amount = read $ args !! 1
            hfile <- lift $ openFile (args !! 2) ReadMode
            cont <- lift $ hGetContents hfile
            -- cont `deepseq` lift $ hClose hfile
            out <- generateSeq amount $ (makeNgram len cont :: M.Map String Int)
            lift $ putStrLn out

usage :: IO ()
usage = putStrLn "Need 3 arguments length, amount and filename"
  