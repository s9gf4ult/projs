module Main where

import System.Clock (Clock(ProcessCPUTime), getTime, toNanoSecs)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Data.Array
-- import Data.Array.Base

-- Matrix multiplication benchmark

type Matrix = Array (Int, Int) Double

main :: IO ()
main = do
  [n] <- getArgs >>= \case
    [a] -> pure ([read a :: Int])
    _ -> exitFailure

  t1 <- clock
  let a = newMatrix n
      b = newMatrix n

  let c' = matrixMult a b

  printf "% 8.6f\n" (c' ! ((n `div` 2), (n `div` 2)) )

  t2 <- clock
  printf "%ds\n" $ (t2 - t1) `div` 1000000000

newMatrix :: Int -> Matrix
newMatrix n =
  let tmp = 1 / fromIntegral n / fromIntegral n :: Double in
  array ((0, 0), (pred n, pred n))
      [((i,j), tmp * fromIntegral(i - j) * fromIntegral (i + j))
          | i <- range (0, pred n),
            j <- range (0, pred n) ]


matrixMult :: Matrix -> Matrix -> Matrix
matrixMult x y    =
  let
    ((li, lj), (ui, uj))  = bounds x
    ((li',lj'),(ui',uj'))  =  bounds y
    bnds = ((li, lj'), (ui, uj'))
  in case uj - lj == ui' - li' of
    False -> error "Size mismatch"
    True -> array bnds $ do
      ij@(i, j) <- range bnds
      let
        a = sum $ do
          (k, k') <- zip (range (lj, uj)) (range (li', ui'))
          return $ x ! (i, k) * y ! (k', j)
      return (ij, a)

clock :: IO Integer
clock = toNanoSecs <$> getTime ProcessCPUTime
