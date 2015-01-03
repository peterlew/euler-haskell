
bestDown :: [[Integer]] -> Integer
bestDown grid = let bd g i j best = if i > 16 then 
                                     if j >= 19 then best
                                     else bd g 0 (j + 1) best
                                    else let prod = (grid!!i)!!j 
                                                  * (grid!!(i + 1))!!j
                                                  * (grid!!(i + 2))!!j
                                                  * (grid!!(i + 3))!!j
                                         in bd g (i + 1) j (max best prod)
                in bd grid 0 0 0

bestRight :: [[Integer]] -> Integer
bestRight grid = let bd g i j best = if j > 16 then 
                                      if i >= 19 then best
                                      else bd g (i + 1) 0 best
                                     else let row = grid!!i
                                              prod = row!!j
                                                   * row!!(j + 1)
                                                   * row!!(j + 2)
                                                   * row!!(j + 3)
                                          in bd g i (j + 1) (max best prod)
                 in bd grid 0 0 0

bestDiagDown :: [[Integer]] -> Integer
bestDiagDown grid = let bd g i j best = if j > 16 then 
                                         if i >= 16 then best
                                         else bd g (i + 1) 0 best
                                        else let prod = (grid!!i)!!j 
                                                      * (grid!!(i + 1))!!(j + 1)
                                                      * (grid!!(i + 2))!!(j + 2)
                                                      * (grid!!(i + 3))!!(j + 3)
                                             in bd g i (j + 1) (max best prod)
                    in bd grid 0 0 0

bestDiagUp :: [[Integer]] -> Integer
bestDiagUp grid = let bd g i j best = if j > 16 then 
                                       if i >= 19 then best
                                       else bd g (i + 1) 0 best
                                      else let prod = (grid!!i)!!j 
                                                    * (grid!!(i - 1))!!(j + 1)
                                                    * (grid!!(i - 2))!!(j + 2)
                                                    * (grid!!(i - 3))!!(j + 3)
                                           in bd g i (j + 1) (max best prod)
                  in bd grid 3 0 0

toNums :: [String] -> [[Integer]]
toNums [] = []
toNums (s:ss) = map read (words s) : toNums ss

bestOfAll :: [[Integer]] -> Integer
bestOfAll grid = maximum [bestDown grid, bestRight grid, bestDiagDown grid, bestDiagUp grid]

main = do
    content <- readFile "assets/p11.txt"
    let ls = lines content
        nums = toNums ls
        best = bestOfAll nums
    return best