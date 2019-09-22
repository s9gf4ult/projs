module Main where

import System.Clock (Clock(ProcessCPUTime), getTime, toNanoSecs)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Array.Repa

newMatrix :: Int -> Array D DIM2 Double
newMatrix n =
  let
    tmp = 1 / fromIntegral n / fromIntegral n
  in fromFunction (ix2 n n) $ \(Z :. i :. j) ->
    tmp * fromIntegral (i - j) * fromIntegral (i + j)

multiply
  :: (Source r1 Double, Source r2 Double)
  => Array r1 DIM2 Double
  -> Array r2 DIM2 Double
  -> Array D DIM2 Double
multiply ar1 ar2 =
  let
    Z :. x1 :. y1 = extent ar1
    Z :. x2 :. y2 = extent ar2
  in case y1 == x2 of
    False -> error "Size mismatch"
    True -> fromFunction (ix2 x1 y2) $ \(Z :. x :. y) -> sum $ do
      k <- [ 0.. pred y1 ]
      return $ ar1 ! (ix2 x k) * ar2 ! (ix2 k y)

main :: IO ()
main = do
  n <- getArgs >>= \case
    [a] -> pure (read a)
    _ -> exitFailure
  t1 <- clock
  let
    a = newMatrix n
    b = newMatrix n
    res = multiply a b
  let center = n `div` 2
  printf "% 8.6f\n" $ res ! (ix2 center center)
  t2 <- clock
  printf "%dns\n" (t2 - t1)

clock :: IO Integer
clock = toNanoSecs <$> getTime ProcessCPUTime
