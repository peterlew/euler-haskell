
import Data.List
import qualified Data.Numbers.Primes as P

oddPrimes :: [Integer]
oddPrimes = tail P.primes

isSumOfPrimeAndTwiceSqr :: Integer -> Bool
isSumOfPrimeAndTwiceSqr n =
    let cs = map f1 (takeWhile (< n) oddPrimes)
        ss = map f2 cs
    in any f3 ss
 where f1 p = div (n - p) 2
       f2 c = sqrt $ fromIntegral c
       f3 s = s == fromIntegral (floor s)

oddComposites :: [Integer]
oddComposites = filter f [9..]
 where f n = odd n && (not . P.isPrime) n