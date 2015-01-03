
import Data.List

allDiffs :: [Int] -> [Int]
allDiffs [] = []
allDiffs (x:xs) = nub (map (\n -> n - x) xs ++ allDiffs xs) 

canAddK :: [Int] -> Int -> Bool
canAddK ns k = if length ns > 2 then k < sum (take 3 ns) && not (elem k $ allDiffs ns)
               else not (elem k $ allDiffs ns)

expandSet :: [Int] -> [[Int]]
expandSet ns = 
    let strt = last ns + 1
        diffs = allDiffs ns
        cap = if length ns > 2 then sum $ take 3 ns else 999999
    in map (\c -> ns ++ [c]) (f strt diffs cap)
 where f c ds cp = 
        if c == cp then []
        else if not $ elem c ds then c : f (c + 1) ds cp
             else f (c + 1) ds cp

allSetsSizeN :: Int -> [[Int]]
allSetsSizeN 1 = map (\n -> [0, n]) [1..]
allSetsSizeN n = concat $ map expandSet (allSetsSizeN (n - 1))
