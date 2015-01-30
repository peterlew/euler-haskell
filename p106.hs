
import qualified Math.Combinatorics.Exact.Binomial as B 

allABArrangements :: Int -> [[String]]
allABArrangements n =
    let arrs 0 b = [replicate b "B"]
        arrs a 0 = [replicate a "A"]
        arrs a b = map ("A":) (arrs (a - 1) b) ++ map ("B":) (arrs a (b - 1))
    in map ("A":) (arrs (n - 1) n)

validAB :: [String] -> Bool
validAB ls = 
    let vab [] cur = cur < 0
        vab (l:ls) cur = if cur < 0 then True
                         else vab ls (cur + if l == "A" then 1 else (-1))
    in vab ls 0

p106 :: Int -> Int
p106 x = sum $ map f [2..(div x 2)]
 where f n = B.choose x (2 * n) * (length $ filter validAB (allABArrangements n))