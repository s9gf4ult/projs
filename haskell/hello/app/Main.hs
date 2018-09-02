module Main where

import Data.ByteString.Builder
import Data.ByteString.Lazy as BL
import Data.List as L
import Lib
import System.Environment

main :: IO ()
main = do
  [a] <- getArgs
  BL.putStr
    $ toLazyByteString
    $ mconcat
    $ L.intersperse (charUtf8 '\n')
    $ countBuilders
    $ unL
    $ allUniqBraces (SeqLen $ read a) $ L [roundBrace]
