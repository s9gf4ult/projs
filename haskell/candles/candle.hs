module Main where

import Data.Monoid

data Candle a b = Cempty 
                | Tick {getTime :: DateTime,
                        getCandleCost :: a,
                        getCandleVolume :: b}
                | Candle {getOpenData :: PeriodData a b,
                          getCloseData :: PeriodData a b,
                          getChildTicks :: [Candle a b]}

data PeriodData a b = PeriodData {getPeriodCost :: a,
                                  getPeriodVolume :: b}

data DateTime = DateTime

instance Monoid Candle where
  mempty = Cempty
  mappend (Tick {getTime 