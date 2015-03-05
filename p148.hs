
import Data.List

mAdd :: Int -> Int -> Int
mAdd x y = mod (x + y) 7

pRow :: [Int] -> [Int]
pRow r = zipWith mAdd (r ++ [0]) (0 : r)

nRow :: Int -> [Int]
nRow n = (iterate pRow [1])!!n

cap :: Integer
cap = 10^9

showTri = do
    sequence $ map f [0..49]
 where f n = print $ nRow n

--what a cool one. If you look at showTri, we get a sierpinski triangle mod 7
--there are 7 by 7 triangles at each stage, 
--so 28 smaller triangles in each big one
--that means that after a power of 7 rows, we have a power of 28 non-zero entries
--the remainder is some fancy arithmetic

fancyCount :: Integer -> Integer
fancyCount r = 
    let bSev = baseSev r 
        bTails = map product $ map (map (+ 1)) $ tails (tail bSev)
    in sum $ zipWith3 f bSev bTails (map (28^) [0..])
 where f n t m = m * t * (div (n * (n + 1)) 2)

baseSev :: Integer -> [Integer]
baseSev 0 = []
baseSev n = let d = mod n 7 in d : baseSev (div (n - d) 7)