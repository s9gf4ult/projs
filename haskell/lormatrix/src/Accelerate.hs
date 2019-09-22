module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Lens (_3, _2)
import Data.Array.Accelerate.Control.Lens ()
import           Data.Array.Accelerate
import           Prelude               (($), (>>=), pure, read)
import Data.Array.Accelerate.LLVM.Native (run)
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

newMatrix :: Int -> Matrix Double
newMatrix n =
  let
    tmp = 1 / P.fromIntegral n / P.fromIntegral n
  in fromFunction (Z :. n :. n) $ \(Z :. i :. j) ->
    tmp * P.fromIntegral (i - j) * P.fromIntegral (i + j)


main :: P.IO ()
main = do
  [n] <- getArgs >>= \case
    [a] -> pure ([read a :: Int])
    _ -> exitFailure

  -- t1 <- clock
  let
    a = newMatrix n
    b = newMatrix n
    c = multiplyMatrixMatrix (use a) (use b)
    center = lift $ n `div` 2
    res = run $ unit $ c ! (index2 center center)
  P.print res
  -- printf "% 8.6f\n" (

  -- t2 <- clock
  -- printf "%ds\n" $ (t2 - t1) `div` 1000000000


-- clock :: P.IO P.Integer
-- clock = toNanoSecs <$> getTime ProcessCPUTime
