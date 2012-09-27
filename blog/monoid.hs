import Data.Monoid
import Test.QuickCheck
import Data.Ratio

data Avg a = Avg {aSum :: a,
                  aCount :: Int}
             deriving (Eq, Show)

mkAvg a = Avg a 1
getAvg (Avg v 0) = Nothing
getAvg (Avg v c) = Just $ v / (fromIntegral c)

instance (Num a) => Monoid (Avg a) where
  mempty = Avg 0 0
  mappend (Avg v c) (Avg vv cc) = Avg (v + vv) (c + cc)
  

avg :: (Fractional a) => [a] -> Maybe a
avg [] = Nothing
avg x = Just ((sum x) / (fromIntegral $ length x))

avg2 x y = do
  ax <- avg x
  ay <- avg y
  return $ (ax + ay) / 2

avg3 x y = do
  let xag = mconcat $ map mkAvg x
      yag = mconcat $ map mkAvg y
  xa <- getAvg xag
  ya <- getAvg yag
  ra <- getAvg $ mappend xag yag
  r <- avg $ concat [x, y]
  return $ (r, ra, (xa + ya) / 2)

checkEqual :: [[Rational]] -> Bool
checkEqual arb = case res of
  Nothing -> True
  Just (a, b) -> a == b
  where
    res = do
      a <- avg $ concat arb
      b <- getAvg $ mconcat $ map mconcat $ map (map mkAvg) arb
      return (a, b)

main = quickCheck checkEqual