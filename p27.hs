
import Useful

consecutivePrimes :: Int -> Int -> Int
consecutivePrimes a b = let f n = n * n + a * n + b
                            cp c = if isPrime $ fromIntegral (f c) then cp (c + 1)
                                   else c
                        in cp 0

p27 :: Int
p27 = let p27' a b best bestProd = if a >= 1000 then bestProd
                                   else if b >= 1000 then p27' (a + 1) (-1000) best bestProd
                                        else let val = consecutivePrimes a b
                                             in if val > best then p27' a (b + 1) val (a * b)
                                                else p27' a (b + 1) best bestProd
      in p27' (-1000) (-1000) 0 0