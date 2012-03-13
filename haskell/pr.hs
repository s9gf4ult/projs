
import qualified Data.List
import qualified Data.Char as Char
-- import Control.Monad.State
import System.Random
import Control.Applicative
import Control.Monad.Writer

data Shape t = Circle t (t, t)
             | Square (t,t) (t, t)
             | Triangle (t, t) (t, t) (t, t)
             | Polygon [(t, t)]
             deriving (Show, Eq, Read, Ord)

data EitherList a b = EEmpty    -- Список элементов дух любых типов !
                    | ELeft a (EitherList a b)
                    | ERight b (EitherList a b)
                    deriving (Show)

eitherSplit x = eith ([], []) x
                where eith ac EEmpty = ac
                      eith (ac1, ac2) (ELeft a aa) = eith ((a:ac1), ac2) aa
                      eith (ac1, ac2) (ERight a aa) = eith (ac1, (a:ac2)) aa

join' a = a >>= id
reverse' x = r [] x
             where r ac [] = ac
                   r ac (x:xs) = r (x:ac) xs

reverse'' = foldl (\a b -> (b:a)) []

area (Circle r _) = 3.14 * (r ^ 2 )
area (Square (x1, y1) (x2, y2)) = abs ((x1 - x2) * (y1 - y2))
area (Triangle v1 v2 v3) = sqrt(p * (p - a) * (p - b) * (p - c))
     where distBetween (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)
           a = distBetween v1 v2
           b = distBetween v1 v2
           c = distBetween v2 v3
           p = (a + b + c) / 2

area (Polygon (v1:v2:v3:vs)) = area (Triangle v1 v2 v3) + area (Polygon (v1:v3:vs))
area (Polygon (v1:v2)) = 0

rightTriangles max perimetr = [(a, b, c) | a <- [1..max], b <- [1..a], c <- [1..b], c^2 + b^2 == a^2, a+b+c == perimetr]

concsd d (x:x':xs) = (show x) ++ d ++ (concsd d (x':xs))
concsd d (x:[]) = (show x)

data Vector a = Vector a a
     deriving Show

addvec (Vector x1 y1) (Vector x2 y2) = (Vector (x1+x2) (y1+y2))

maximum' (a:[]) = a
maximum' (x:xs)
         | x > maxt = x
         | otherwise = maxt
         where maxt = maximum' xs

maxfn _ [a] = a
maxfn fn (x:xs)
      | fn x exte = x
      | otherwise = exte
      where exte = maxfn fn xs

replicate' n x
           | n <= 0 = []
           | otherwise = x:replicate' (n - 1) x

take' n [a] | n <= 0 = []
            | otherwise = [a]
take' n (x:xs) | n <= 0 = []
               | otherwise = x:take' (n - 1) xs

-- reverse' [] = []
-- reverse' (x:xs) = (reverse' xs) ++ [x]

zip' (x:[]) (y:ys) = (x, y):[]
zip' (x:xs) (y:[]) = (x, y):[]
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

qsort _ [] = []
qsort fn (x:xs) = (qsort fn lesser) ++ [x] ++ (qsort fn bigger)
                  where (lesser, bigger) = Data.List.partition (fn x) xs

collatz :: (Integral a) => a -> [a]
collatz x = th: collatz th
            where th | even x = x `div` 2
                     | otherwise = (x*3)+1

accerman m n = acc' 1 m n
               where
               acc' a 0 n = (a, n+1)
               acc' a m 0 = acc' (a+1) (m-1) 1
               acc' a m n = let aa = acc' (a+1) m (n-1)
                            in acc' ((fst aa) + 1) (m-1) (snd aa)


uniqueseq [a] = [a]
uniqueseq (x:xs) = x : (takeWhile (/= x) ( uniqueseq xs))

enumerate x = en' 0 x
              where en' _ [] = []
                    en' n (y:ys) = (n, y) : en' (n+1) ys

foldl' _ p [] = p
foldl' fn p (x:xs) = foldl' fn (fn p x) xs

foldr' _ p [] = p
foldr' fn p (x:xs) = fn x (foldr' fn p xs)

avg a b = (a+b)/2

length' x = ln 0 x
            where ln n [] = n
                  ln n (y:ys) = ln (n+1) ys

findseq :: (Eq a) => [a] -> [a]
findseq x = solve 2
            where solve l
                    | l > (length $ take l x) = []
                    | (length $ fsq $ take l x) > 0 = fsq $ take l x
                    | otherwise = solve (l + 1)
                  fsq tl = ffsq 1 tl
                  ffsq n tt
                    | n*2 > length tt = []
                    | back n tt == subback n tt = back n tt
                    | otherwise = ffsq (n+1) tt
                  back u uu = drop (length uu - u) uu
                  subback o oo = take o $ drop (length oo - (2*o)) oo

mulEven :: (Integral a) => a -> a -> Maybe a
mulEven x y | (even x) && (even y) = Just (x*y)
            | otherwise = Nothing

data BTree a = BEmpty
             | Node a (BTree a) (BTree a)
             deriving (Show)

instance Functor BTree where
         fmap _ BEmpty = BEmpty
         fmap fn (Node a l r) = Node (fn a) (fmap fn l) (fmap fn r)

singleton x = Node x BEmpty BEmpty

findBtree x BEmpty = Nothing
findBtree x (Node a l r) | x == a = Just a
                         | x > a = findBtree x r
                         | otherwise = findBtree x l

class Tofu t where
      tofu :: b a -> t a b

data Frank a b = Frank (b a) deriving (Show)

instance Tofu Frank where
         tofu a = Frank a


insertBtree x BEmpty = singleton x
insertBtree x (Node a l r) | x == a = Node x l r
                           | x > a = Node a l $ insertBtree x r
                           | otherwise = Node a (insertBtree x l) r

stackCalc x = calc [] x
              where calc acum [] = head acum
                    calc (ac1:ac2:acs) (y:ys) | y == "+" = calc ((ac1+ac2):acs) ys
                                              | y == "-" = calc ((ac1-ac2):acs) ys
                                              | y == "*" = calc ((ac1*ac2):acs) ys
                                              | y == "/" = calc ((ac1/ac2):acs) ys
                    calc acum (y:ys) = calc ((read y):acum) ys

stackCalcFoldl x = foldl foldfn [] x
                   where foldfn rt st | st == "+" = (((rt !! 0) + (rt !! 1)):(tail $ tail rt))
                                      | st == "-" = (((rt !! 0) - (rt !! 1)):(tail $ tail rt))
                                      | st == "*" = (((rt !! 0) * (rt !! 1)):(tail $ tail rt))
                                      | st == "/" = (((rt !! 0) / (rt !! 1)):(tail $ tail rt))
                                      | otherwise = ((read st):rt)

stackCalcFoldl' x = foldl foldfn [] x
                    where foldfn (a:b:bs) "*" = (a+b):bs
                          foldfn (a:b:bs) "/" = (a/b):bs
                          foldfn (a:b:bs) "+" = (a+b):bs
                          foldfn (a:b:bs) "-" = (a-b):bs
                          foldfn ac smth = (read smth):ac

data Direction = Goleft | Goright | Forward deriving Show
data Position = Top | Bottom deriving Show

-- pathVariants :: [(a, a, a)] -> (a, [Direction])
pathVariants x = pvs (0, []) Bottom x
                 where pvs (len, paths) _ [] = (len, reverse paths)
                       pvs (len, paths) Bottom ((a, b, c):ps) | c <= (a+b) = pvs (len+c, (Forward:paths)) Bottom ps
                                                              | otherwise = pvs (len+a+b, (Forward:Goleft:paths)) Top ps
                       pvs (len, paths) Top ((a, b, c):ps) | a <= (b+c) = pvs (len+a, (Forward:paths)) Top ps
                                                           | otherwise = pvs (len+b+c, (Forward:Goright:paths)) Bottom ps

class Fluffy f where
  furry :: (a -> b) -> f a -> f b
 
-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry f [] = []
  furry f (x:xs) = ((f x):(furry f xs))
 
-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f Nothing = Nothing
  furry f (Just x) = Just (f x)
 
-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f x = \a -> f (x a)

newtype EitherLeft b a = EitherLeft (Either a b) deriving Show
newtype EitherRight a b = EitherRight (Either a b) deriving Show
  
-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft $ Left (f x)
  furry f (EitherLeft (Right x)) = EitherLeft $ Right x
 
-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Left x)) = EitherRight $ Left x
  furry f (EitherRight (Right x)) = EitherRight $ Right (f x)
 
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f x = banana (unicorn . f) x
 
-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana f [] = []
  banana f (x:xs) = (f x) ++ (banana f xs)
  unicorn x = [x]
 
-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f Nothing = Nothing
  banana f (Just x) = f x
  unicorn = Just
 
-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana f x a = f (x a) a
  unicorn x = \ _ -> x
 
-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x)) = f x
  banana f (EitherLeft (Right x)) = (EitherLeft (Right x))
  unicorn = EitherLeft . Left
 
-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Left x)) = (EitherRight (Left x))
  banana f (EitherRight (Right x)) = f x
  unicorn = EitherRight . Right
 
-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> (m a)
jellybean m = banana id m
 
-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple ma mf = banana (\f ->  (furry' f ma)) mf
 
-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy as amb = foldr cf (unicorn []) $ map amb as
               -- cf :: m b -> m [b] -> m [b]
               where cf mb mbs = apple mbs (furry' (:) mb)
 
-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage mas = moppy mas id 
 
-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f ma mb = apple mb (furry' f ma)
 
-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f ma mb mc = apple mc (apple mb (furry' f ma))
 
-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f ma mb mc md = apple md (apple mc (apple mb (furry' f ma)))
 
newtype State s a = State {
  state :: (s -> (s, a))
}
 
-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
-- furry :: (a -> b) -> ma -> mb
--       :: (a -> b) -> (State (\s -> (s, a))) -> (State (\s -> (s, b)))
  furry f (State s) = State (\x -> let (ss, aa) = s x
                                       in (ss, (f aa)))
 
-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
-- banana:: (a -> m b) -> m a -> m b
--       :: (a -> (State (s -> (s, b)))) -> (State (s -> (s, a))) -> (State (s -> (s, b)))
  banana f (State s) = jellybean (State (\x -> let (ss, aa) = s x
                                                   in (x, (f aa))))
  unicorn a = State (\s -> (s, a))
                                                           
-- getRandoms from to = do
--                      a <- newStdGen
--                      ((randomRs (from, to) a !! 0, randomRs (from, to) a !! 1, randomRs (from, to) a !! 2):(getRandoms from) to)

-- main = getLine >>= return . show . (foldr (:) [1]) . takeWhile (/= 1) . collatz . read >>= putStrLn
-- main = print $ show $ Data.List.nub [findseq $ collatz a | a <- [1..10000]]
-- main = print $ show $ enumerate $ [length $ uniqueseq $ collatz a | a <- [1..10000]]
-- main = print $ show $ (foldl avg (0 :: Rational) $ map fromInteger $ take 100500 $ collatz 3) == (foldr avg (0 :: Rational) $ map fromInteger $ take 100500 $ collatz 3)
-- main = do m <- getLine
--           n <- getLine
--           putStrLn $ show $ accerman (read m) (read n)

-- main = do putStrLn "Give some input"
--           l <- getLine
--           if l == "bye" then putStrLn "bye !"
--           else do putStrLn $ map Char.toUpper l
--                   main


main = getLine >>= return . map (Char.toUpper) >>= putStrLn







