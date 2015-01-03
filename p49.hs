
import Useful
import Data.List
import qualified Data.Numbers.Primes as P

candidatePrimes :: [Int]
candidatePrimes = dropWhile (< 1000) $ takeWhile (< 10000) P.primes

arePermutations :: Int -> Int -> Int -> Bool
arePermutations p1 p2 p3 = 
    let s1 = f p1
    in s1 == f p2 && s1 == f p3
 where f = sort . show

satisfies :: Int -> Int -> Bool
satisfies p1 p2 = 
    let p3 = 2 * p2 - p1
    in P.isPrime p3 && arePermutations p1 p2 p3

validTrips :: [(Int, Int, Int)]
validTrips = [(p1, p2, 2 * p2 - p1) | p1 <- candidatePrimes, p2 <- candidatePrimes, p1 < p2, satisfies p1 p2]

p49 :: String
p49 = foldrTriple (++) "" (mapTriple show (validTrips!!1))

