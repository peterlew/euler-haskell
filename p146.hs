
import Data.List
import qualified Data.Numbers.Primes as P

cap :: Integer
cap = 150 * 10^6

test :: Integer -> Bool
test n = let nSqr = n^2
             adds = [1, 3, 7, 9, 13, 27]
         in (all P.isPrime $ map (\k -> nSqr + k) adds) &&
            (not $ any P.isPrime $ map (\k -> nSqr + k) ([1..27]\\adds))

cands :: [Integer]
cands = filter f [1..cap-1]
 where f n = all (\p -> mod n p > 0) [3, 7, 13]

--this will eliminate MOST numbers efficiently, 
--but some false positives will sneak through
checkNum :: Integer -> Bool
checkNum n =
    let adds = [1, 3, 7, 9, 13, 27]
        rems p = map (\x -> mod x p) adds
        pCands = takeWhile (<= div n 2) P.primes 
        works p = let pRem = mod n p
                   in all (> 0) $ map (\r -> mod (pRem^2 + r) p) (rems p)
    in all works pCands

main = do
    let res = filter checkNum cands
        res2 = filter test res
    print res
    print res2
    print $ sum res2

