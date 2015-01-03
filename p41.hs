
import Useful
import Data.List

nPandigitals :: Int -> [Integer]
nPandigitals n = map read (permutations (foldr (++) "" (map show [1..n])))

nPrimePandigitals :: Int -> [Integer]
nPrimePandigitals n = filter isPrime $ nPandigitals n