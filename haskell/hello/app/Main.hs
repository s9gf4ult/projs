module Main where

import           Data.List               as L
import           GHC.Stack
import           Stack
import Data.Foldable

printStack :: HasCallStack => IO ()
printStack = do
  for_ (getCallStack callStack) $ \ (fn, src) -> do
    putStrLn fn

printHello :: IO ()
printHello = putStrLn "Hello"

main :: IO ()
main = do
  putStrLn ">>>>>>>"
  printStack
  putStrLn ">>>>>>>"
  withRankN $ printStack
  putStrLn ">>>>>>>"
  withoutRankN $ printStack
  putStrLn ">>>>>>>"
  withRankN $ withRankN $ printStack
  putStrLn ">>>>>>>"
  withRankN printHello
  putStrLn ">>>>>>>"
  withRankN $ withoutRankN $ withRankN printStack
