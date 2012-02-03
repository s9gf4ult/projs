
import qualified Data.List

data Shape t = Circle t (t, t)
             | Square (t,t) (t, t)
             | Triangle (t, t) (t, t) (t, t)
             | Polygon [(t, t)]
             deriving Show


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

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

zip' (x:[]) (y:ys) = (x, y):[]
zip' (x:xs) (y:[]) = (x, y):[]
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

qsort _ [] = []
qsort fn (x:xs) = (qsort fn lesser) ++ [x] ++ (qsort fn bigger)
                  where lesser = filter (fn x) xs
                        bigger = filter (not . fn x) xs

collatz x = th: collatz th
            where th | even x = x `div` 2
                     | otherwise = (x*3)+1

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
                
                      


main = print $ show $ Data.List.nub [findseq $ collatz a | a <- [1..10000]]
-- main = print $ show $ enumerate $ [length $ uniqueseq $ collatz a | a <- [1..10000]]
-- main = print $ show $ (foldl avg (0 :: Rational) $ map fromInteger $ take 100500 $ collatz 3) == (foldr avg (0 :: Rational) $ map fromInteger $ take 100500 $ collatz 3)