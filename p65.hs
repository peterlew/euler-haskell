
import Data.List
import Useful

type Frac = (Integer, Integer)

eFracList :: Integral a => [a]
eFracList = 2 : foldr (++) [] (map f [1..])
 where f n = [1, 2 * n, 1]

fAdd :: Frac -> Frac -> Frac
fAdd (n1, d1) (0, _)   = (n1, d1)
fAdd (0, _)   (n2, d2) = (n2, d2)
fAdd (n1, d1) (n2, d2) = let dv = lcm d1 d2
                         in (n1 * (div dv d1) + n2 * (div dv d2), dv)

reciprocal :: Frac -> Frac
reciprocal (n, d) = (d, n)

nthConvergent :: Int -> Frac
nthConvergent 1 = (2, 1)
nthConvergent n =
    let xs = reverse $ take n eFracList
    in foldl f (0, 0) xs
 where f res x = fAdd (x, 1) (reciprocal res)

p65 :: Int
p65 = sum $ map cToD (show $ fst $ nthConvergent 100)



