module Main where

import           Data.Array.Accelerate
import qualified Prelude               as P

multiplyMatrixMatrix
  :: (Num a)
  => Acc (Matrix a)
  -> Acc (Matrix a)
  -> Acc (Matrix a)
multiplyMatrixMatrix x y =
  let
    Z :. rows :. _cols  = unlift ((shape x) :: Exp DIM2 )
    Z :. _rows :. cols  = unlift ((shape y) :: Exp DIM2 )
  in
    fold1 (+) $ transpose $
         zipWith (*)
            (replicate (lift $ Any :. All :. All :. cols) x)
            (replicate (lift $ Any :. rows :. All :. All) y)

main :: P.IO ()
main = error "FIXME: main not implemented"
