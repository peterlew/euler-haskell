
import qualified Data.Numbers.Primes as P

divideAway :: Integral a => a -> a -> a
divideAway n d = if mod n d == 0 then divideAway (div n d) d else n

numPrimeFacs :: Integral a => a -> Int
numPrimeFacs n =
    let npf 1 _ c = c
        npf n (p:ps) c = if p > floor (sqrt (fromIntegral n)) then (c + 1)
                         else if mod n p == 0 then npf (divideAway (div n p) p) ps (c + 1)
                              else npf n ps c  
    in npf n P.primes 0

areNConsecWithNPrimeFacs :: Int -> Int -> Bool
areNConsecWithNPrimeFacs x n = all f [0..(n-1)]
 where f k = numPrimeFacs (x + k) == n

p47 :: Int
p47 = head $ filter f [1..]
 where f n = areNConsecWithNPrimeFacs n 4 