
import Useful (squareRoot)
import qualified Data.Numbers.Primes as P 

maxP :: Integer
maxP = 1000000

p131 :: [Integer]
p131 = filter f $ map g [1..(squareRoot (div maxP 3))]
 where f p = p < maxP && P.isPrime p
       g n = 3 * n^2 + 3*n + 1