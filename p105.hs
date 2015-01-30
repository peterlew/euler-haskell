
import qualified Math.Combinatorics.Exact.Binomial as B 
import Data.List
import Data.List.Split
import Useful (lst, leave)

passesCriterion2 :: [Int] -> Bool
passesCriterion2 xs =
 let l = length xs
     pc2 1 = True
     pc2 k = (sum (take k xs) > sum (leave (k - 1) xs)) && pc2 (k - 1)
 in pc2 (l - 1)

passesCriterion1 :: [Int] -> Bool
passesCriterion1 xs = 
    let as = map ((flip subsetsSizeN) xs) [1..(div (length xs) 2)]
    in all f as
 where f ns = length ns == length (nub (map sum ns))

--huh, subsetsSizeN really is just another way to do genIncLsts. More general though
subsetsSizeN :: Int -> [a] -> [[a]]
subsetsSizeN n xs | length xs < n = []
subsetsSizeN 1 xs = map lst xs
subsetsSizeN n (x:xs) = map (\l -> x:l) (subsetsSizeN (n - 1) xs) ++ 
                        subsetsSizeN n xs

isSpecialSet :: [Int] -> Bool
isSpecialSet xs = passesCriterion2 xs && passesCriterion1 xs

main = do 
    handle <- readFile "assets/p105.txt"
    let ls = lines handle
        lls = map (splitOn ",") ls
        ns :: [[Int]]
        ns = map (map read) lls
        sortNs = map sort ns
        spec = filter isSpecialSet sortNs
    print (sum (map sum spec))