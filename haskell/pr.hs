
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
