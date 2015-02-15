
import qualified Data.Numbers.Primes as P 

aOfN :: Integer -> Integer
aOfN n | mod n 2 == 0 = 3
aOfN n | mod n 5 == 0 = 3
aOfN n =
    let f cur tenPw = if cur == 0 then 1
                      else 1 + f (mod (cur + tenPw) n) (mod (10 * tenPw) n)
    in f 1 10 

allTwosAndFives :: Integer -> Bool
allTwosAndFives x = divideAway (divideAway x 2) 5 == 1
 where divideAway n p = if mod n p > 0 then n else divideAway (div n p) p

willNeverFactor :: Integer -> Bool
willNeverFactor p = not $ allTwosAndFives (aOfN p)

