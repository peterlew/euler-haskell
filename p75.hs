
import Useful
import qualified Data.Map as M

--using Euclid's formula for generating a primitive Pythagorean triple,
--a = m^2 - n^2, b = 2*m*n, c = m^2 + n^2
--L = 2 * m^2 + 2 * m * n
--for m, n be naturals, n < m, gcd m n == 1, m - n even
--n is at least 1 so 2*m^2 + 2 m <= upperL

upperL :: Int
upperL = 1500000

upperM :: Int
upperM = length $ takeWhile (< upperL) (map f [1..])
 where f n = 2 * n^2 + 2 * n

markAllForM :: Int -> M.Map Int Int -> M.Map Int Int
markAllForM m mp' =
    let twoMSqr = 2 * m^2
        mafm [] mp = mp
        mafm (n:ns) mp 
         | gcd m n /= 1 = mafm ns mp
         | even (m - n) = mafm ns mp
         | otherwise = let tot = twoMSqr + 2 * m * n
                       in if tot > upperL then mp
                          else mafm ns (markAllMultiples tot mp)
    in mafm [1..(m-1)] mp'
 where markAllMultiples c m = foldl f m (map (* c) [1..(div upperL c)])
       f res k = M.insertWith (+) k 1 res

markAllForLs :: M.Map Int Int
markAllForLs = foldr markAllForM M.empty [2..upperM]

p75 :: Int
p75 = M.size $ M.filter (== 1) markAllForLs