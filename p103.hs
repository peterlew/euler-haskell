
import qualified Math.Combinatorics.Exact.Binomial as B 
import Data.List
import Useful

passesCriterion2 :: [Int] -> Bool
passesCriterion2 xs =
 let l = length xs
     pc2 0 = True
     pc2 k = (sum (take k xs) > sum (drop (l - k + 1)  xs)) && pc2 (k - 1)
 in pc2 l

passesCriterion1 :: [Int] -> Bool
passesCriterion1 xs = 
    let a2 = allTwos xs
        a3 = allThrees xs
    in f a2 && f a3
 where f ns = length ns == length (nub (map sum ns))

allTwos :: [Int] -> [[Int]]
allTwos [] = []
allTwos (x:xs) = map (\n -> [x, n]) xs ++ allTwos xs

allThrees :: [Int] -> [[Int]]
allThrees xs | length xs < 3 = []
allThrees (x:xs) = map (\l -> x:l) (allTwos xs) ++ allThrees xs

isSpecialSet :: [Int] -> Bool
isSpecialSet xs = passesCriterion2 xs && passesCriterion1 xs

genIncLsts :: Int -> Int -> Int -> [[Int]]
genIncLsts 1 lwr upr = map lst [lwr..upr]
genIncLsts n lwr upr | upr - lwr + 1 < n = [[]]
genIncLsts n lwr upr | upr - lwr + 1 == n = [[lwr..upr]]
genIncLsts n lwr upr = map (\ls -> lwr:ls) (genIncLsts (n - 1) (lwr + 1) upr) ++ 
                       genIncLsts n (lwr + 1) upr 

