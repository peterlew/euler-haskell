
import qualified Math.Combinatorics.Exact.Binomial as B  

p53 :: Int
p53 = length $ filter (> 1000000) [B.choose n r | n <- [1..100], r <- [1..100], r <= n]