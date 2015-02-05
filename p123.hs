
import qualified Data.Numbers.Primes as P 

rOfN :: Int -> Integer
rOfN n = 
    let p = P.primes!!(n-1)
    in mod ((p - 1)^n + (p + 1)^n) (p^2)

cap :: Integer
cap = 10^10

p123 :: Int
p123 = 
    let g n p = mod ((p - 1)^n + (p + 1)^n) (p^2)
        f n (p:ps) = if g n p > cap then n
                     else f (n + 1) ps
    in f 1 P.primes
