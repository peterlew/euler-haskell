
import Useful (count)
import qualified Data.Numbers.Primes as P 

diffsCaseTopCorner :: Int -> [Int]
diffsCaseTopCorner k = [
                        6 * k - 1,
                        6 * k + 1,
                        12 * k + 5
                       ]

diffsCaseCorner :: Int -> Int -> [Int]
diffsCaseCorner k n = [
                       6 * (k - 1) + div n k,
                       6 * k + div n k - 1,
                       6 * k + div n k + 1
                      ]

diffsCaseLast :: Int -> [Int]
diffsCaseLast k = let n = 6 * k - 1
                  in [
                      n,
                      12 * k - 7,
                      6 * k + div n k,
                      6 * k + div n k + 1
                     ]

has3Primes :: [Int] -> Bool
has3Primes xs = count P.isPrime xs == 3

has3InRow :: Int -> [Integer]
has3InRow 1 = [2]
has3InRow k = 
    let k' :: Integer
        k' = fromIntegral k
        strt = 3 * k' * (k' - 1) + 2
        frst = if has3Primes $ diffsCaseTopCorner k then [strt] else []
        snd = filter (> 0) $ map f (map (*k) [1..5])
        f n = if has3Primes $ diffsCaseCorner k n then strt + fromIntegral n else 0
        last = if has3Primes $ diffsCaseLast k then [3 * k' * (k' + 1) + 1] else []
    in frst ++ snd ++ last

allHas3 :: [Integer]
allHas3 = concat ( [1] : map has3InRow [1..])

