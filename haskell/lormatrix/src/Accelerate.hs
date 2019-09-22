module Main where

import Control.Lens
import Data.Array.Accelerate.Control.Lens ()
import           Data.Array.Accelerate
import           Prelude               (($))
import qualified Prelude               as P

multiplyMatrixMatrix
  :: forall a. (Num a)
  => Acc (Matrix a)
  -> Acc (Matrix a)
  -> Acc (Matrix a)
multiplyMatrixMatrix x y =
  let
    Z :. rows :. _cols  = unlift (shape x) :: Z :. Exp Int :. Exp Int
    Z :. _rows :. cols  = unlift (shape y) :: Z :. Exp Int :. Exp Int
  in
    fold1 (+) $ transposeOn _2 _3 $
         zipWith (*)
            ((replicate (lift $ Any :. All :. All :. cols) x) :: Acc (Array DIM3 a))
            ((replicate (lift $ Any :. rows :. All :. All) y) :: Acc (Array DIM3 a))



main :: P.IO ()
main = error "FIXME: main not implemented"
