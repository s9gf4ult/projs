{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module NGramm where

import System.Random
import Control.Arrow ((&&&))
import Control.Monad
import Data.List
import Data.Text (pack, unpack, replace)

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

instance AsAtom [Atom] where
  atom = id

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


substitute :: String -> Substitution -> String
substitute str (Substitution from to) = unpack $ replace (pack from) (pack to) (pack str)


getRandomElement :: (Random a, Num a, Ord a) => [(a, b)] -> IO (Maybe b)
getRandomElement [] = return Nothing
getRandomElement xs = do
  el <- randomRIO (0, probSum xs)
  return $ Just $ snd $ head $ filter (\((l, g), ret) -> el >= l && el <= g) lowHighList
  where
    -- probSum :: (Ord a, Num a) => [(a, b)] -> a
    probSum = sum . map fst

    -- lowHighList :: (Random a, Num a, Ord a) => [((a, a), b)]
    lowHighList = map (((\x -> (probSum x) - (last $ map fst x)) &&& probSum) &&& (last . map snd)) $
                   tail $ inits xs

generate :: Rule -> [Substitution] -> IO ()
generate r subs = do fs <- flattenRule r
                     putStrLn $ foldl substitute fs subs

  
flattenRule :: Rule -> IO String
flattenRule (Rule repls) = do elt <- getRandomElement $ map (replProbability &&& replSequence) repls
                              liftM concat $ mapM id $ map flattenAtom $ fromM elt
                                where fromM Nothing = []
                                      fromM (Just x) = x

flattenAtom :: Atom -> IO String
flattenAtom (AtomS s) = return  s
flattenAtom (AtomR r) = flattenRule r

autograph  = rule $ variant1

variant1   = rule $ sequence1 <+> (20 <:> "" <|> 80 <:> sequence2) <+>
             (80 <:> "" <|> 20 <:> ("продолжайте участвовать в конкурсах. " <|> "участвуйте в дальнейших конкурсах. ")) <+> (90 <:> "" <|> 10 <:> "ура!")

sequence1  = rule $ respect <+> appeal <+> name <+> " " <+> fromAuthor <+> wish
respect    = rule $ "" <|> "многоуважаемому " <|> "уважаемому "
appeal     = rule $ "" <|> colleague
colleague  = rule $ (30 <:> "коллеге " <|> 30 <:> "товарищу " <|> 30 <:> "соратнику " <|> 10 <:> "камраду ") <+>
                    ("" <|> "по " <+> ("цеху " <|> "поприщу ") <+> fp)
fp         = rule $ "программирования " <|> "ФП " <|> "функционального программирования "
name       = rule $ "%1"
fromAuthor = rule $ "" <|> "от автора "
wish       = rule $ "на добрую память" <+> (". " <|> "и с наилучшими пожеланиями. ")

sequence2  = rule $ ("" <|> "желаю ") <+> ("успехов " <|> "удачи ") <+> ("на профессиональном поприще. " <|> "в области программирования. ")
