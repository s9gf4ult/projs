module Main where

import System.Clock (Clock(ProcessCPUTime), getTime, toNanoSecs)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Data.Array.Unboxed
import Data.Array.Base

-- Matrix multiplication benchmark

type Matrix = UArray (Int, Int) Double

main :: IO ()
main = do
  [n] <- getArgs >>= \case
    [a] -> pure ([read a :: Int])
    _ -> exitFailure

  t1 <- clock
  let a = newMatrix n
      b = newMatrix n

  let c' = matrixMult a $ transpose b

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


transpose :: Matrix -> Matrix
transpose x = array resultBounds [((j,i), x!(i,j))
                                     | i <- range (li,ui),
                                       j <- range (lj,uj) ]
  where ((li,lj),(ui,uj))     =  bounds x
        resultBounds          =  ((lj,li),(uj,ui))

matrixMult :: Matrix -> Matrix -> Matrix
matrixMult x y    =  array resultBounds [((i,j),
                                         let basei = rowIndex x i
                                             basej = rowIndex y j
                                         in sum [unsafeAt x ( basei + k ) * unsafeAt y ( basej + k )
                                                    | k <- range (lj,uj) ]
                                         )
                                            | i <- range (li,ui),
                                              j <- range (li',ui') ]

  where ((li,lj),(ui,uj))         =  bounds x
        ((li',lj'),(ui',uj'))     =  bounds y
        resultBounds | (lj,uj)==(lj',uj')    =  ((li,li'),(ui,ui'))
                     | otherwise             = error "matMult: incompatible bounds"
        rowIndex  arr n           = index (bounds arr) (n,0)

clock :: IO Integer
clock = toNanoSecs <$> getTime ProcessCPUTime
