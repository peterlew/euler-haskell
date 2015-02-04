
import qualified Math.Combinatorics.Exact.Binomial as B 

waysToXwithD :: Int -> Int -> Integer
waysToXwithD x d = 
    let f n = B.choose (fromIntegral (x - n * (d - 1))) (fromIntegral n)
    in sum $ map f [1..(div x d)]

totalWays :: Int -> Integer
totalWays x = sum $ map (waysToXwithD x) [2..4]