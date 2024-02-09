module Lib
    ( run
    ) where

import Options.Applicative

data Opts = Opts
  { maxCores :: Integer
  }

optsInfo :: ParserInfo Opts
optsInfo = info p (progDesc "Cores / jobs count calculator")
  where
    p = Opts <$> option auto (long "cores" <> short 'c')

searchCores :: Opts -> [(Integer, Integer)]
searchCores o = (Prelude.error "FIXME: not implemented")


run :: IO ()
run = do
  p <- execParser optsInfo
  print $ searchCores p
