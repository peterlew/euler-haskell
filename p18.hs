
import qualified Data.Map as M

toNumWords :: [String] -> [[Int]]
toNumWords [] = []
toNumWords (s:ss) = map read (words s) : toNumWords ss

bestPath :: [[Int]] -> Int
bestPath grid = let sze = length grid - 1
                    bp i j mp = if i < 0 then maybe 0 id (M.lookup (0, 0) mp)
                                else if j < 0 then bp (i - 1) (i - 1) mp
                                     else case M.lookup (i, j) mp of
                                           Just val -> bp i (j - 1) mp
                                           Nothing -> bp i (j - 1) (M.insert (i, j) (max (gt (i + 1) j mp) (gt (i + 1) (j + 1) mp) + (grid!!i)!!j) mp)
                in bp sze sze M.empty
    where gt i j mp = maybe 0 id (M.lookup (i, j) mp)

main = do
    cnt <- readFile "assets/p67.txt"
    let ls = lines cnt
        nums = toNumWords ls
        best = bestPath nums
    return best