module Main where

import Data.Monoid
import Data.Time

data Candle a = Cempty 
              | Tick {tickTime :: LocalTime,
                      tickCost :: a,
                      tickVolume :: a}
              | Candle {candleOpenCost :: a,
                        candleCloseCost :: a,
                        candleMinCost :: a,
                        candleMaxCost :: a,
                        candleVolume :: a,
                        candleOpenTime :: LocalTime,
                        candleCloseTime :: LocalTime,
                        childTicks :: [Candle a]}


instance Monoid (Candle a) where
  mempty = Cempty
  mappend Cempty x = x
  mappend x Cempty = x
  mappend (Tick time cost volume) (Candle {candleOpenCost = opentime,
                                           candleCloseTime = closetime
                                           