
import Useful

isTruncatablePrime :: Int -> Bool
isTruncatablePrime n = isPrime n && isTPLeft n && isTPRight n
 where isTPLeft n  = if n < 10 then True 
                     else let nxtN = read (tail $ show n)
                          in isPrime nxtN && isTPLeft nxtN
       isTPRight n = if n < 10 then True
                     else let nxtN = div n 10
                          in isPrime nxtN && isTPRight nxtN

allOddDigits :: String -> Bool
allOddDigits s = all (\x -> mod x 2 == 1) (map cToD s)

criterion1 :: Int -> Bool
criterion1 n = f (show n)
 where f ('2':s) = allOddDigits s
       f s = allOddDigits s

truncPrimes :: [Int]
truncPrimes = filter f [11..]
 where f x = criterion1 x && isTruncatablePrime x