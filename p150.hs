
import Data.List

ss :: [Integer]
ss = map g $ take 500501 $ iterate f 0
 where f t = mod (615949 * t + 797807) (2^20)
       g t = t - (2^19)

triangle :: [[Integer]]
triangle = 
    let f r ls = if r > 1000 then []
                 else take r ls : f (r + 1) (drop r ls)
    in f 1 (tail ss)

nextRow :: [Integer] -> [[Integer]] -> [[Integer]] -> [[Integer]]
nextRow vs r1 r2 = zipWith4 f vs r1 (tail r1) (tail r2) 
 where f v rs1 rs2 rs3 = map (+ v) $ zipWith3 g (0:rs1) (0:rs2) (0:0:rs3)
       g t1 t2 t3 = t1 + t2 - t3

findMin :: Integer
findMin = 
    let f rowNum rNxt rNxtNxt bst =
         if rowNum < 1 then bst else 
          let vs = triangle!!(rowNum - 1)
              row = nextRow vs rNxt rNxtNxt
              res = min bst (minimum $ map minimum row)
          in f (rowNum - 1) row rNxt res
    in f 1000 (zeros 1001) (zeros 1002) 0
 where zeros n = replicate n []  

main = do
    print findMin
