
import Data.List (elemIndices, permutations)
import qualified Data.Set as S
import Data.List (sort)


data Material = Plastic
              | Iron
              | Wood
              | Glass
              | Copper deriving (Eq, Show, Ord)

data Basket = Basket { balls :: ![Material],
                       material :: !Material } deriving (Eq, Show)

instance Ord Basket where
  compare a b = (material a) `compare` (material b)

isSyntethic :: Material -> Bool
isSyntethic Plastic = True
isSyntethic Glass = True
isSyntethic _ = False

haveSameBalls :: Basket -> Bool
haveSameBalls Basket {balls = b,
                      material = m} = elem m b

isMetal :: Material -> Bool
isMetal Iron = True
isMetal Copper = True
isMetal _ = False

isOk :: [Basket] -> Bool
isOk b = and [noOneHaveSameBalls,
              woodHaveNoPlastic,
              metalHave1Wood1Glass,
              coperHaveSyntetic,
              someHave1IronAnd1Plasitc,
              plasticHave1Coper]
  where
    noOneHaveSameBalls = and $ map (not . haveSameBalls) b
    woodHaveNoPlastic = and $ map wnhp b
    wnhp Basket { balls = bls,
                  material = m} | m == Wood = not $ elem Plastic bls
                                | otherwise = True
    metalHave1Wood1Glass = let x = concat $ map balls $ filter (isMetal . material) b
                           in ((length $ elemIndices Wood x) == 1) &&
                              ((length $ elemIndices Glass x) == 1)
    -- mhwg Basket { balls = bls,
    --               material = m} | isMetal m = ((length $ elemIndices Wood bls) == 1) &&
    --                                           ((length $ elemIndices Glass bls) == 1)
    --                             | otherwise = True
    coperHaveSyntetic = and $ map chsyn b
    chsyn Basket { balls = bls,
                   material = m} | m == Copper = any isSyntethic bls
                                 | otherwise = True
    someHave1IronAnd1Plasitc = any oneionep b
    oneionep Basket { balls = bls,
                      material = m} = ((length $ elemIndices Iron bls) == 1) &&
                                      ((length $ elemIndices Plastic bls) == 1)
    plasticHave1Coper = and $ map p1c b
    p1c Basket {balls = bls,
                material = m} | m == Plastic = (length $ elemIndices Copper bls) == 1
                              | otherwise = True


makeGroups :: Int -> [a] -> [[[a]]]
makeGroups _ [] = []
makeGroups count elems | count > (length elems) = []
                       | otherwise = map group $ sequence $ replicate (length elems) [0..(count - 1)]
  where
    group idxs = foldr gfun (replicate count []) $ zip idxs [0..]
    gfun (idx, pst) state = repladd idx (elems !! pst) state
    repladd idx elem l = map (\(x, y) -> if y == idx then (elem : x) else x) $ zip l [0..]

makeUnorderedGroups :: (Ord a) => Int -> [a] -> [[[a]]]
makeUnorderedGroups count elems = S.toList $ (foldl toset S.empty) $ makeGroups count elems
  where
    toset s grps = S.insert (map sort grps) s

makeBaskets :: [Material] -> [Material] -> [[Basket]]
makeBaskets bsk bl = map makebsk $ makeUnorderedGroups (length bsk) bl
  where
    makebsk groups = map (\(bs, bl) -> Basket {balls = bl, material = bs}) $ zip bsk groups


basket = [Plastic, Iron, Wood, Glass, Copper]
ball = concat $ replicate 2 basket

solve = filter isOk $ makeBaskets basket ball

showSolution :: [[Basket]] -> [String]
showSolution x = ("There is " ++ (show $ length x) ++ " solutions:") : (concat $ map shw $ zip x [1..])
  where
    shw (bskts, idx) = ("Solution " ++ (show idx) ++ ":") : (map ("   " ++) $ map showbasket bskts)
    showbasket b = (show $ material b) ++ " basket with: " ++ (show $ balls b)

main = mapM_ putStrLn $ showSolution $ solve