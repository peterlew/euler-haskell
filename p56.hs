
import Useful

p56 :: Int
p56 = maximum $ map f [a^b | a <- [1..99], b <- [1..99]]
 where f n = foldr (+) 0 (map cToD (show n))