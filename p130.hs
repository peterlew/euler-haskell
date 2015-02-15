
import Data.List
import qualified Data.Numbers.Primes as P 

aOfN :: Integer -> Integer
aOfN n | mod n 2 == 0 = 0
aOfN n | mod n 5 == 0 = 0
aOfN n =
    let f cur tenPw = if cur == 0 then 1
                      else 1 + f (mod (cur + tenPw) n) (mod (10 * tenPw) n)
    in f 1 10 

hasProp :: Integer -> Bool
hasProp n | mod n 2 == 0 = False
hasProp n | mod n 5 == 0 = False
hasProp n | P.isPrime n = False
hasProp n = mod (n - 1) (aOfN n) == 0