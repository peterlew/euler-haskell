
import Useful (squareRoot, isSquare)
import Data.List
import qualified Data.Map as M 

maxPQR :: Integer
maxPQR = 120000

maxPQ :: Integer
maxPQ = maxPQR 

--every triangle has a p, q, r that intersect at a 3x 120 degree angle
--this forms three interior integral-sided triangles pqa, prb, qrc
--they obey the cosine law, so p^2 + q^2 + p * q = a^2, etc.

--that means we need three intersecting pairs of p and q that satisfy
--p^2 + q^2 + p * q = a^2

--substituting x = q - p, y = p + q, z = 2 * a, we have 
--x^2 + 3 * y^2 = z^2

maxX :: Integer
maxX = maxPQ

maxY :: Integer
maxY = maxPQ

--this formula has two parametrizations, 
--1: x = +- s^2 - 3t^2, y = 2 * s * t
--2: x = +- s^2 + t^2 + 4 * s * t, y = s^2 - t^2

--the bounds are a bit tricky, and I fudged them a little...
--the pairMapBrute uses a simple brute force algorithm and was used
--to make sure the pairMapFancy was correct, up to about maxPQR = 5000

candXYZ1Prim :: [(Integer, Integer, Integer)]
candXYZ1Prim = 
    let tsPairs = [
         (s^2 - 3 * t^2, 2 * s * t) |
         t <- [1..maxT],
         s <- [if maxX < 3 * t^2 then squareRoot (3 * t^2 - maxX) else 1..
               squareRoot (3 * t^2 + maxX)],
         gcd s t == 1,
         mod (s - t) 2 == 1,
         mod s 3 /= 0
                  ]
        xOrNegX = map (\(x, y) -> (abs x, y)) tsPairs
        xyzs = [ (x, y, squareRoot (x^2 + 3 * y^2)) | (x, y) <- xOrNegX ]
    in filter f xyzs
 where maxT = div maxY 2
       f (x, y, z) = y > x && y < maxPQ

candXYZ2Prim :: [(Integer, Integer, Integer)]
candXYZ2Prim =
    let tsPairs = [  
         (s^2 + t^2 + 4 * s * t, s^2 - t^2) |
         s <- [-maxS..maxS]\\[0],
         --pretty sure on the bound below
         let cap = min (abs s) (squareRoot (3 * s^2 + maxX) - 2 * s ),
         t <- if s < 0 then [1..cap] else [-cap..(-1)],
         gcd s t == 1,
         mod (s - t) 2 == 1,
         mod s 3 /= mod t 3
                  ]
        xOrNegX = map (\(x, y) -> (abs x, y)) tsPairs
        xyzs = [ (x, y, squareRoot (x^2 + 3 * y^2)) | (x, y) <- xOrNegX ]
    in filter f xyzs
 --here's the bound I'm not sure about. Increase it if we miss results
 where maxS = div maxX 2
       f (x, y, z) = y > x && y < maxPQ && y > 0 && x > 0

candXYZ :: [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
candXYZ c = 
    let expandMults ((x, y, z), (ox, oy, oz)) = 
         if y >= maxPQ then []
         else (x, y, z) : expandMults ((x + ox, y + oy, z + oz), (ox, oy, oz))
    in concat $ map expandMults (zip c c)

validXYZ :: [(Integer, Integer, Integer)] -> [(Integer, Integer, Integer)]
validXYZ c = filter f (candXYZ c)
 where f (x, y, z) = mod z 2 == 0 && y < maxPQ

validXY :: [(Integer, Integer, Integer)] -> [(Integer, Integer)]
validXY c = map f (validXYZ c)
 where f (x, y, z) = (x, y)

xyToAB :: (Integer, Integer) -> (Integer, Integer)
xyToAB (x, y) = (div (y - x) 2, div (y + x) 2)

validAB :: [(Integer, Integer, Integer)] -> [(Integer, Integer)]
validAB c = filter f $ map xyToAB (validXY c)
 where f (a, b) = a <= b && a > 0 && b > 0 && a + b < maxPQ

allABs :: [(Integer, Integer)]
allABs = nub $ sort (validAB candXYZ1Prim ++ validAB candXYZ2Prim)

validPQ :: (Integer, Integer) -> Bool
validPQ (p, q) = isSquare (p^2 + q^2 + p * q)

validPQR :: (Integer, Integer, Integer) -> Bool
validPQR (p, q, r) = validPQ (p, q) &&
                     validPQ (p, r) &&
                     validPQ (q, r)

pairList :: [(Integer, [Integer])]
pairList = map f [1..maxPQR]
 where f p = (p, filter (g p) [1..maxPQR])
       g p q = validPQ (p, q)

pairMapBrute :: M.Map Integer [Integer]
pairMapBrute = M.filter (/= []) $ foldr f M.empty [1..maxPQR]
 where f p m = M.insert p (filter (g p) [1..(maxPQR - p - 1)]) m
       g p q = validPQ (p, q)

pairMapFromList :: [(Integer, Integer)] -> M.Map Integer [Integer]
pairMapFromList ps = 
    let pmfl [] m = m
        pmfl ((p, q):ps) m = pmfl ps $
                             M.insertWith (++) p [q] $
                             M.insertWith (++) q [p] m
    in pmfl ps M.empty

pairMapFancy :: M.Map Integer [Integer]
pairMapFancy = M.map sort $ pairMapFromList allABs
    
--once we have our pairs, we need to find the 3-cliques, or triangles

trianglesAt :: Integer -> M.Map Integer [Integer] -> [(Integer, Integer, Integer)]
trianglesAt p m =
    let adjs = m M.! p
        f k = map (\a -> (p, k, a)) $ intersect (m M.! k) adjs 
    in concat $ map f adjs

hitMap :: M.Map Integer [(Integer, Integer, Integer)]
hitMap = M.filter (/= []) $
         M.mapWithKey f pairMapFancy
 where f k a = trianglesAt k pairMapFancy

hits :: [(Integer, Integer, Integer)]
hits = nub $ map sortTup $ M.foldr (++) [] hitMap
 where sortTup (a, b, c) = 
        let [d, e, f] = sort [a, b, c]
        in (d, e, f)

p143 :: Integer
p143 = sum $ nub $ filter (<= maxPQR) $ map addTup hits
 where addTup (a, b, c) = a + b + c

main = do
    print p143
