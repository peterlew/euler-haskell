
import Useful
import Data.List

primes :: [Int]
primes = filter isPrime [2..]

test :: String -> Bool
test ('0':s) = False
test s = all f [0..6]
 where f n = mod (read $ drop (n + 1) $ take (n + 4) s) (primes!!n) == 0

p43 :: Integer
p43 = foldr (+) 0 (map read (filter test $ permutations "0123456789"))