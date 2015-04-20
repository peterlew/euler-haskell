
import Useful (squareRoot)

m :: Integer
m = 10^8

totalSum :: Integer
totalSum = (m - 1) + div (m * (m + 1)) 2 + realSum + compSum

realSum :: Integer
realSum = sum $ map f [2..(div m 2)]
 where f n = n * (div m n - 1)

compSum :: Integer
compSum = sum $ map f [1..squareRoot (div m 2)]
 where f n = sum $ map (g n) [n..squareRoot (m - n^2)]
       g n k = contrForAB (n, k)

contrForAB :: (Integer, Integer) -> Integer
contrForAB (a, b) | gcd a b > 1 = 0
contrForAB (a, b) = 
    let d = a^2 + b^2
        amt = if a == b then 2 * a else 2 * (a + b)
    in sum $ map (\k -> amt * k * (div m (k * d))) [1..div m d]

main = do
    print totalSum