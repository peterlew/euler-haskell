
import Useful (isSquare, squareRoot)

test :: Integer -> Bool
test n = isSquare (5 * n^2 + 8 * n + 4) || 
         isSquare (5 * n^2 - 8 * n + 4)

bNuggets :: [Integer]
bNuggets = filter test (map (*2) [1..])
         
--there appears to be a pattern

bNuggets2 :: [Integer]
bNuggets2 = 
    let bn n | n < 1 = 0
        bn n = 18 * (bn (n - 1)) - (bn (n - 2))
               + if mod n 2 == 0 then (-16) else 16
    in map bn [1..] 

--seems legit

bNuggets3 :: [Integer]
bNuggets3 = 16 : zipWith3 f bNuggets3 (0 : bNuggets3) sixteens
 where f n1 n2 s = 18 * n1 - n2 + s
       sixteens = map (*16) $ scanl (*) (-1) (repeat (-1))

--mmm haskelliscious 

lOfB :: Integer -> Integer -> Integer
lOfB b par = squareRoot ((div b 2)^2 + (b + par)^2)

lNuggets :: [Integer]
lNuggets = zipWith lOfB bNuggets3 (scanl (*) (-1) (repeat (-1)))