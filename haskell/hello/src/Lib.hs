module Lib where

import Control.Applicative
import Control.Monad.State
import Data.ByteString.Builder
import Data.HashSet as H
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

closedBraces :: Brace -> Builder
closedBraces (Brace o c) = charUtf8 o <> charUtf8 c

braceWrap :: Builder -> Brace -> Builder
braceWrap s (Brace o c) = charUtf8 o <> s <> charUtf8 c

dec :: Gen a -> Gen a
dec ma = do
  s <- get
  guard $ s > 0
  put $ pred s
  ma

allBraces :: [Brace] -> Gen Builder
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
      return $ a <> b

runGen :: SeqLen -> Gen a -> [a]
runGen s ma = evalStateT ma s

-- uniqBuilder :: [Builder] -> [Builder]
-- uniqBuilder = go H.empty
--   where
--     go _acc [] = []
--     go acc (a:as) = case H.member b acc of
--       True  -> go acc as
--       False -> (lazyByteString b) : go (H.insert b acc) as
--       where b = toLazyByteString a

uniqBuilder :: [Builder] -> [Builder]
uniqBuilder bs = fmap lazyByteString $ H.toList $ H.fromList $ toLazyByteString <$> bs
