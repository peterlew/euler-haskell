
import qualified Data.Numbers.Primes as P

--the first ~550 primes add to about one million, so that's the largest possible chain

summedChainsOfLenghthN :: Int -> [Int]
summedChainsOfLenghthN n = 
    takeWhile (< 1000000) (map f [0..])
 where f d = foldr (+) 0 (take n $ drop d P.primes)

p50 :: Int
p50 = let p50' n = case filter P.isPrime $ summedChainsOfLenghthN n of 
                    [] -> p50' (n - 1)
                    [x] -> x
      in p50' 550

