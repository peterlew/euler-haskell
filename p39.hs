
import Data.List

--given a and p, c^2 - b^2 = a^2 and c + b = p - a
-- b = sqrt(c^2 - a^2), c + sqrt(c^2 - a^2) = p - a
-- c^2 - a^2 = (p - a - c)^2
-- c^2 - a^2 = p^2 - pa - pc - ap + a^2 + ac - cp + ac + c^2
-- -2 * a^2 = p^2 - 2 * pa - 2 * pc + 2 * ac
-- 2 * pa - 2 * a^2 - p^2 = 2 * c * (a - p)
-- c = (2 * (pa - a^2) - p^2) / (a - p)

evalC :: Int -> Int -> Int
evalC a p = div (2 * (p * a - a^2) - p^2) (2 * (a - p))

isRightTriangle :: Int -> Int -> Bool
isRightTriangle a p = 
    let c = evalC a p
        b = floor $ sqrt $ fromIntegral (c^2 - a^2)
    in a + b + c == p && a^2 + b^2 == c^2

solutionCount :: Int -> Int
solutionCount p = div (length $ filter ((flip isRightTriangle) p) [1..(div p 2 - 1)]) 2

p39 :: Int
p39 = 
    let allSols = map solutionCount [0..1000]
        maxVal = maximum allSols
    in maybe 0 id $ elemIndex maxVal allSols