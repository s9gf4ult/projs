
module Main where
import Data.Ratio
import System.Environment
import Control.Monad

pinumber a = 1 / ((2 * (toRational (sqrt 2)) / 9801) * (sum $ map sumfunc [1..a]))
  where sumfunc k = (fac (4 * k)) * (1103 + (26390 * k)) / (((fac k)^4) * (396 ^ (4 * (round k))))
        fac x = product [1..x]

main = do
  args <- getArgs
  when (length args == 1) $ do 
    putStrLn $ show $ fromRational $ pinumber ((read $ args !! 0) % 1)