
import qualified Data.Numbers.Primes as P 

bigOnesNum :: Integer
bigOnesNum = 10^9

aOfN :: Integer -> Integer
aOfN n | mod n 2 == 0 = 3
aOfN n | mod n 5 == 0 = 3
aOfN n =
    let f cur tenPw = if cur == 0 then 1
                      else 1 + f (mod (cur + tenPw) n) (mod (10 * tenPw) n)
    in f 1 10 

primeFactOfBig :: Integer -> Bool
primeFactOfBig p = mod bigOnesNum (aOfN p) == 0