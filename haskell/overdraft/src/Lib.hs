module Lib where

import Control.Monad
import Data.Foldable
import Data.Functor
import Data.List as L
import Data.Set qualified as S
import Data.Traversable
import Factor.Prime (trialDivision)
import Options.Applicative

data Opts = Opts
  { maxCores :: Integer
  , minCores :: Integer
  , maxJobs :: Integer
  , minJobs :: Integer
  , maxCpus :: Integer
  , minCpus :: Integer
  , cpus :: Integer
  , limit :: Int
  }

optsInfo :: ParserInfo Opts
optsInfo = info p (progDesc "Cores / jobs count calculator")
  where
    p = Opts
      <$> option auto (long "max-cores" <> value 1024)
      <*> option auto (long "min-cores" <> value 1)
      <*> option auto (long "max-jobs" <> value 1024)
      <*> option auto (long "min-jobs" <> value 1)
      <*> option auto (long "max-cpus" <> value 1024)
      <*> option auto (long "min-cpus" <> value 1)
      <*> option auto (long "cpus" <> short 'c')
      <*> option auto (long "limit" <> value 10)

-- | Peek each element of the list and returns the rest
peek :: [a] -> [(a, [a])]
peek = \case
  [] -> []
  (a:as) -> (a, as) : (peek as <&> \(r, res) -> (r, a:res))

interchange :: [a] -> [a] -> [a]
interchange (a:as) (b:bs) = a:b:(interchange as bs)
interchange a [] = a
interchange [] b = b

divisions :: Integer -> [(Integer, Integer)]
divisions cpus = go 1 $ fst $ trialDivision cpus
  where
    go !acc primes = [(acc, prod primes)] ++ do
      ((prime, pwr), rest) <- peek primes
      let
        newPwr = pwr - 1
        newPrimes = if newPwr > 0
          then (prime, newPwr) : rest
          else rest
      go (acc * prime) newPrimes
    prod p = product $ p <&> \(prime, pwr) -> prime ^ pwr

searchCores :: Opts -> [(Integer, (Integer, Integer))]
searchCores o = do
  cpus <- interchange [o.cpus..o.maxCpus] (reverse [o.minCpus .. o.cpus - 1])
  (jobs, cores) <- S.toAscList $ S.fromList $ divisions cpus
  guard $ jobs >= o.minJobs && jobs <= o.maxJobs
  guard $ cores >= o.minCores && cores <= o.maxCores
  pure (cpus, (jobs, cores))


run :: IO ()
run = do
  o <- execParser optsInfo
  for_ (take o.limit $ searchCores o) $ print
