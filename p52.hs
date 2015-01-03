
import Data.List

sameDigits :: [Int] -> Bool
sameDigits xs = all f (tail xs)
 where f x = sort (show x) == sort (show $ head xs)

p52 :: Maybe Int
p52 = find f [1..]
 where f n = sameDigits (map (* n) [1..6])