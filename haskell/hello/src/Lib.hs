module Lib where

import Control.Applicative
import Control.Monad.State
import Data.Foldable
import Data.Set as S

data Brace = Brace
  { brOpen  :: Char
  , brClose :: Char
  } deriving (Eq, Ord)

roundBrace :: Brace
roundBrace = Brace '(' ')'

squareBrce :: Brace
squareBrce = Brace '[' ']'

newtype SeqLen = SeqLen Int
  deriving (Eq, Ord, Enum, Num)

type Gen a = StateT SeqLen [] a

closedBraces :: Brace -> String
closedBraces (Brace o c) = [o, c]

braceWrap :: String -> Brace -> String
braceWrap s (Brace o c) = o:s ++ [c]

dec :: Gen a -> Gen a
dec ma = do
  s <- get
  guard $ s > 0
  put $ pred s
  ma

allBraces :: [Brace] -> Gen String
allBraces braces = closed <|> wrapped <|> appended
  where
    closed = dec $ lift $ closedBraces <$> braces
    wrapped = do
      s <- dec $ allBraces braces
      lift $ braceWrap s <$> braces
    appended = do
      a <- dec $ allBraces braces
      modify succ
      b <- allBraces braces
      return $ a ++ b

runGen :: SeqLen -> Gen a -> [a]
runGen s ma = evalStateT ma s

lazyUniq :: (Ord a) => [a] -> [a]
lazyUniq as = evalState (foldrM go [] as) S.empty
  where
    go a acc = do
      s <- get
      case S.member a s of
        True  -> pure acc
        False -> do
          put $ S.insert a s
          pure $ a:acc
