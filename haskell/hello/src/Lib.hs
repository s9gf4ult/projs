module Lib where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 as BL
import Data.HashSet as H
import Data.Hashable
import Data.List as L
import Data.Map.Strict as M
import Data.Traversable

data Brace = Brace
  { brOpen  :: Char
  , brClose :: Char
  } deriving (Eq, Ord)

roundBrace :: Brace
roundBrace = Brace '(' ')'

squareBrce :: Brace
squareBrce = Brace '[' ']'

newtype SeqLen = SeqLen Int
  deriving (Eq, Ord, Show, Enum, Num)

type Gen a = StateT SeqLen [] a

newtype L a = L { unL :: [a] }
  deriving ( Functor, Applicative, Alternative, Monad
           , Semigroup, Monoid, Show)

type Res = BL.ByteString

closedBraces :: Brace -> Res
closedBraces (Brace o c) = BL.pack [o, c]

braceWrap :: Res -> Brace -> Res
braceWrap s (Brace o c) = BL.singleton o <> s <> BL.singleton c

dec :: Gen a -> Gen a
dec ma = do
  s <- get
  guard $ s > 0
  put $ pred s
  ma

allBraces :: [Brace] -> Gen Res
allBraces braces = go
  where
    go = closed <|> wrapped <|> appended
      where
        closed = dec $ lift $ closedBraces <$> braces
        wrapped = do
          s <- dec $ allBraces braces
          lift $ braceWrap s <$> braces
        appended = do
          a <- dec $ go
          modify succ
          b <- go
          return $ a <> b

type SeqAccum = Map SeqLen (L Res)

getFromAcc :: SeqAccum -> SeqLen -> L Res
getFromAcc acc s = maybe mempty id $ M.lookup s acc

allUniqBraces :: SeqLen -> L Brace -> L Res
allUniqBraces maxLen braces = go singles 2
  where
    go :: SeqAccum -> SeqLen -> (L Res)
    go acc top
      | top > maxLen = mconcat $ M.elems acc
      | otherwise =
        let
          wrappedCombinations :: L Res
          wrappedCombinations = do
            a <- getFromAcc acc $ pred top
            brace <- braces
            return $ braceWrap a brace
          res = appendCombinations acc top <> wrappedCombinations
        in go (M.insert top res acc) $ succ top
    singles = M.singleton 1 $ closedBraces <$> braces

appendCombinations :: SeqAccum -> SeqLen -> L Res
appendCombinations acc top = uniq $ do
  resList <- for (sums top) $ \lenSumList -> do
    combs :: [Res] <- for lenSumList $ \len -> do
      getFromAcc acc len
    return $ mconcat combs
  L resList

sums :: SeqLen -> [[SeqLen]]
sums t = L.filter (\a -> L.length a > 1) $ go t
  where
  go top
    | top > 0 = do
        h <- [1..top]
        rest <- go $ top - h
        return $ h : rest
    | otherwise = [[]]

uniq :: (Hashable a, Eq a) => L a -> L a
uniq = L . H.toList . H.fromList . unL

runGen :: SeqLen -> Gen a -> [a]
runGen s ma = evalStateT ma s

uniqBuilder :: [Res] -> [Builder]
uniqBuilder bs = fmap lazyByteString $ H.toList $ H.fromList bs

countBuilders  :: [Res] -> [Builder]
countBuilders bs = fmap f $ sortOn snd $ M.toList $ M.fromListWith (+) $ L.zip bs (L.repeat 1)
  where
    f (b, c) = intDec c <> " - " <> lazyByteString b
