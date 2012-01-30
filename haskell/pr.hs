
import Text.Printf        
data Shape = Circle Radius Vertex
           | Square Vertex Vertex
           | Triangle Vertex Vertex Vertex
           | Polygon [Vertex]
           deriving Show

type Radius = Float
type Vertex = (Float, Float)

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

main = printf "%f" (area (Polygon [(1, 0), (0.8, 0.2), (0.6, 0.4), (0.2, 0.8), (0, 1), (0, 0)]) :: Float)