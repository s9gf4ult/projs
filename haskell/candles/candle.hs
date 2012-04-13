
module Main where

import Data.Monoid
import Data.Time
import Data.Maybe
import Data.Set (Set, singleton, insert, union, toList)
import Data.List (foldl')
import Data.Fixed
import Control.Applicative
import System.Random

data Candle time a = Cempty 
                   | Tick {tickTime :: !time,
                           tickCost :: !a,
                           tickVolume :: !a}
                   | Candle {candleOpenCost :: !a,
                             candleCloseCost :: !a,
                             candleMinCost :: !a,
                             candleMaxCost :: !a,
                             candleVolume :: !a,
                             candleOpenTime :: !time,
                             candleCloseTime :: !time,
                             childTicks :: ! (Set (Candle time a))}
                   deriving (Eq)

                                            
instance (Eq a, Ord t) => Ord (Candle t a) where
  compare Cempty Cempty = EQ
  compare Cempty _ = LT
  compare _ Cempty = GT
  compare (Tick {tickTime = t1}) (Tick {tickTime = t2}) = compare t1 t2
  compare (Tick {tickTime = t1}) (Candle {candleOpenTime = t2}) = compare t1 t2
  compare (Candle {candleOpenTime = t1}) (Tick {tickTime = t2}) = compare t1 t2
  compare (Candle {candleOpenTime = o1,
                   candleCloseTime = c1})
          (Candle {candleOpenTime = o2,
                   candleCloseTime = c2}) = if oo == EQ then compare c1 c2 else oo
    where oo = compare o1 o2
          
          
instance (Eq a, Ord a, Num a, Ord t) => Monoid (Candle t a) where
  mempty = Cempty
  mappend Cempty x = x
  mappend x Cempty = x
  mappend x@(Tick {tickTime = t1,
                   tickCost = c1,
                   tickVolume = v1})
          y@(Tick {tickTime = t2,
                   tickCost = c2,
                   tickVolume = v2}) | x > y = mappend y x
                                     | otherwise = Candle {candleOpenCost = c1,
                                                           candleCloseCost = c2,
                                                           candleMinCost = min c1 c2,
                                                           candleMaxCost = max c1 c2,
                                                           candleVolume = v1 + v2,
                                                           candleOpenTime = t1,
                                                           candleCloseTime = t2,
                                                           childTicks = insert x $ singleton y}
  mappend x@(Tick {tickTime = t1,
                   tickCost = c1,
                   tickVolume = v1})
          y@(Candle {candleOpenCost = oc,
                     candleCloseCost = cc,
                     candleMinCost = minc,
                     candleMaxCost = maxc,
                     candleVolume = v2,
                     candleOpenTime = ot,
                     candleCloseTime = ct,
                     childTicks = childs}) = Candle {candleOpenCost = if t1 < ot then c1 else oc,
                                                     candleCloseCost = if t1 > ct then c1 else cc,
                                                     candleMinCost = min minc c1,
                                                     candleMaxCost = max maxc c1,
                                                     candleVolume = v2 + v1,
                                                     candleOpenTime = min t1 ot,
                                                     candleCloseTime = max t1 ct,
                                                     childTicks = insert x childs}
  mappend x@(Candle {}) y@(Tick {}) = mappend y x
  mappend x@(Candle {candleOpenCost = oc1,
                     candleCloseCost = cc1,
                     candleMinCost = minc1,
                     candleMaxCost = maxc1,
                     candleVolume = v1,
                     candleOpenTime = ot1,
                     candleCloseTime = ct1,
                     childTicks = child1})
          y@(Candle {candleOpenCost = oc2,
                     candleCloseCost = cc2,
                     candleMinCost = minc2,
                     candleMaxCost = maxc2,
                     candleVolume = v2,
                     candleOpenTime = ot2,
                     candleCloseTime = ct2,
                     childTicks = child2}) = Candle {candleOpenCost = if ot2 < ot1 then oc2 else oc1,
                                                     candleCloseCost = if ct2 > ct1 then cc2 else cc1,
                                                     candleMinCost = min minc1 minc2,
                                                     candleMaxCost = max maxc1 maxc2,
                                                     candleVolume = v1 + v2,
                                                     candleOpenTime = min ot1 ot2,
                                                     candleCloseTime = max ot1 ot2,
                                                     childTicks = union child1 child2}

data CandleColor = RedCandle
                 | GreenCandle
                 | GrayCandle
                 deriving (Show, Read)

getCandleColor :: (Ord a) => Candle t a -> Maybe CandleColor
getCandleColor Cempty = Nothing
getCandleColor Tick {} = Nothing
getCandleColor Candle {candleOpenCost = oc,
                       candleCloseCost = cc} = Just $ case compare oc cc of
  EQ -> GrayCandle
  LT -> GreenCandle
  GT -> RedCandle

wah :: Int -> IO ()
wah count = do gen <- newStdGen
               let rnds = randomRs (0, (59 * (fromInteger $ resolution $ (0 :: Pico)))) gen
               time <- getZonedTime
               let times = map (loctime $ zonedTimeToLocalTime time) rnds
               let costs = randomRs (100, 200 :: Int) gen
               let volumes = randomRs (10, 20) gen
               let candles = map (\(t, c, v) -> (Tick t c v)) $ take count $ zip3 times costs volumes
               -- putStrLn $ show $ length candles
               let cnd = foldl' mappend mempty candles
               putStrLn $ fromMaybe "Undefined" $ getCandleColor cnd >>= return . show
               putStrLn $ "open is " ++ (show $ candleOpenCost cnd)
               putStrLn $ "close is " ++ (show $ candleCloseCost cnd)
               putStrLn $ "begin is " ++ (show $ candleOpenTime cnd)
               putStrLn $ "end is " ++ (show $ candleCloseTime cnd)
               putStrLn $ "low is " ++ (show $ candleMinCost cnd)
               putStrLn $ "high is " ++ (show $ candleMaxCost cnd)
               putStrLn $ "child ticks " ++ (show $ length $ toList $ childTicks cnd)
  where
    loctime :: LocalTime -> Int -> LocalTime
    loctime time val = time {localTimeOfDay = (localTimeOfDay time) {todSec = toEnum val}}

main :: IO ()
main = do
  a <- getLine >>= return . read
  wah a