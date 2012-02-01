
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
