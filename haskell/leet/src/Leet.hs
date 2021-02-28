{- |
Copyright: (c) 2021 Aleksey Uymanov
SPDX-License-Identifier: MIT
Maintainer: Aleksey Uymanov <s9gf4ult@gmail.com>

See README for more info
-}

module Leet where


import           Control.Monad
import           Data.Foldable
import           Data.List     as L

sublistSums :: Int -> [Integer] -> [(Integer, [[Integer]])]
sublistSums _ [] = pure (0, [])
sublistSums k l = do
  n <- [1..k]
  let
    (h, t) = splitAt n l
    hlen = length h
  let
    mx = maximum h
    lmx = take hlen $ repeat mx
    hsum = toInteger hlen * mx
  (restSum, restTail) <- sublistSums k t
  return (hsum + restSum, lmx : restTail)
