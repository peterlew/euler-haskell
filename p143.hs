
import Useful (squareRoot, isSquare)
import Data.List
import qualified Data.Map as M 

maxPQR :: Integer
maxPQR = 4000

maxPQ :: Integer
maxPQ = maxPQR 

maxX :: Integer
maxX = maxPQR

maxY :: Integer
maxY = maxPQR

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
         let cap = min (abs s) (squareRoot (3 * s^2 + maxX) - 2 * s ),
         t <- [-cap..cap]\\[0],
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
                             