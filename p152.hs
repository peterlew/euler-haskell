
import Useful
import Data.List

type Frac = (Integer, Integer)

fReduce :: Frac -> Frac
fReduce (n, d) = let g = gcd n d in (div n g, div d g)

fAdd :: Frac -> Frac -> Frac
fAdd (n1, d1) (n2, d2) = fReduce (d2 * n1 + d1 * n2, d1 * d2)

fSub :: Frac -> Frac -> Frac
fSub (n1, d1) (n2, d2) = fReduce (d2 * n1 - d1 * n2, d1 * d2)

fLT :: Frac -> Frac -> Bool
fLT (n1, d1) (n2, d2) = n1 * d2 < n2 * d1

cap :: Integer
cap = 80

--OK, this was a stretch for sure
--From the examples, a solution never includes a number that 
--doesn't divide 2^3 * 3^2 * 5 * 7. Who knows.
--That didn't work all the way, though. I added in one 11, and didn't find any
--But there were some results that included 13. Turns out adding in 13 works...
--the only candidates we need to consider divide 2^3 * 3^2 * 5 * 7 * 13
cands :: [Integer]
cands = sort $ filter f [ (2^a) * (3^b) * (5^c) * (7^d) * (11^e) *
                          (13^f) * (17^g) * (19^h) |
                   a <- [0..3],
                   b <- [0..2],
                   c <- [0..1],
                   d <- [0..1],
                   e <- [0..0],
                   f <- [0..1],
                   g <- [0..0],
                   h <- [0..0]]
 where f n = n >= 2 && n <= cap

fInvList :: [Frac]
fInvList = map f cands 
 where f n = (1, n^2)

fMostFrom :: Frac -> Frac
fMostFrom k = sumList!!((elemIndices k fInvList)!!0)

fSum :: [Frac] -> Frac
fSum fs = foldr fAdd (0, 1) fs 

sumList :: [Frac]
sumList = reverse (tail $ scanl fAdd (0, 1) (reverse fInvList))

fSqrt :: Frac -> Integer
fSqrt (_, d) = squareRoot d

waysToNwith :: Frac -> [Frac] -> [[Integer]]
waysToNwith n [] = []
waysToNwith n (k:ks) = if k == n then [[fSqrt k]] ++ waysToNwith n ks
                       else if fLT (fMostFrom k) n then []
                            else if fLT n (1, 6400) then []
                                 else if fLT n k then waysToNwith n ks
                                      else map (\ls -> fSqrt k : ls) (waysToNwith (fSub n k) ks) ++ waysToNwith n ks                                      

main = do 
    let res = map (\l -> [2, 3] ++ l) $ waysToNwith (5, 36) (drop 2 fInvList)
    let freqs = map (\c -> count (elem c) res) cands
    print $ length res
    print $ zip cands freqs