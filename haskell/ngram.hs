{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module NGramm where

import System.Random
import Control.Arrow
import Control.Monad
import Data.List

-- average :: Fractional a => [a] -> a
-- average l = sum l / (fromIntegral $ length l)
  
-- getRandomElement :: [a] -> IO a
-- getRandomElement l = do i <- randomRIO (0, length l - 1)
--                         return $ l !! i

-- getRandomElementWithProbabilities :: (Ord b, Num b, Random b) => [(a, b)] -> IO a
-- getRandomElementWithProbabilities l = (head . goodList) `liftM` randomRIO (1, sumProbs l)
--   where
--     goodList p = map fst $
--                  dropWhile (\(_, p') -> p' < p) $
--                  map ((fst . last) &&& sumProbs) $
--                  tail $ inits l
--     sumProbs = sum . map snd



newtype Rule = Rule 
               {
                 unwrapRule :: [Replacement]
               }
             deriving Eq

instance Show Rule where
  show = show . unwrapRule

data Replacement = Replacement
                   {
                     replProbability :: Int,
                     replSequence    :: [Atom]
                   }
                 deriving Eq
                          
instance Show Replacement where
  show (Replacement p a) = "(" ++ show p ++ ", " ++ show a ++ ")"

data Atom = AtomS String
          | AtomR Rule
  deriving Eq

instance Show Atom where
  show (AtomS s) = "\"" ++ s ++ "\""
  show (AtomR r) = show r


class AsAtom a where
  atom :: a -> [Atom]

class AsRule r where
  rule :: r -> Rule

infixl 3 <+>
(<+>) :: (AsAtom a, AsAtom b) => a -> b -> [Atom]
(<+>) a b = atom a ++ atom b

infix 2 <:>
(<:>) :: AsAtom a => Int -> a -> Replacement
v <:> a = Replacement v (atom a)

infixl 1 <|>
(<|>) :: (AsRule a, AsRule b) => a -> b -> Rule
a <|> b = Rule (unwrapRule (rule a) ++ unwrapRule (rule b))

instance AsAtom Atom where
  atom a = [a]

instance AsAtom String where
  atom a = [AtomS a]

instance AsAtom Rule where
  atom a = [AtomR a]

instance AsRule String where
  rule = rule . AtomS

instance AsRule Atom where
  rule a = rule [a]

instance AsRule [Atom] where
  rule = rule . Replacement 100

instance AsRule (Int, Atom) where
  rule (p, a) = rule (p, [a])

instance AsRule (Int, [Atom]) where
  rule (p, as) = rule $ Replacement p as

instance AsRule Replacement where
  rule rs = Rule [rs]

instance AsRule [Replacement] where
  rule = Rule

instance AsRule Rule where
  rule = id

data Substitution = Substitution
                    {
                      substFrom :: String,
                      substTo   :: String
                    }

  

-- instance AsAtom Atom where
--   atom a = [a]

-- instance AsRule Atom where
--   atom (AtomR r) = r
--   atom (AtomS s) = 
  