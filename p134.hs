
import qualified Data.Numbers.Primes as P 

extendedEuclidean :: Integral a => a -> a -> (a, a, a)
extendedEuclidean a b =
    let ee r prevR s prevS t prevT = 
         let q = div prevR r
             nxtR = prevR - q * r
             nxtS = prevS - q * s
             nxtT = prevT - q * t
         in if nxtR == 0 then (s, t, r)
            else ee nxtR r nxtS s nxtT t
    in ee b a 0 1 1 0

modInverse :: Integral a => a -> a -> a
modInverse a m = 
    let (x, q, d) = extendedEuclidean a m
    in mod x m

sOfPs :: Integer -> Integer -> Integer
sOfPs p1 p2 = 
    let rem1 = p2 - p1
        mult = 10 ^ (length $ show p1)
        rem2 = mod mult p2
        rem2Inv = modInverse rem2 p2
        hd = mod (rem2Inv * rem1) p2
    in mult * hd + p1

p134 :: Integer
p134 = 
    let f (p:ps) = if p > 1000000 then 0
                   else sOfPs p (head ps) + f ps
    in f (drop 2 P.primes)
