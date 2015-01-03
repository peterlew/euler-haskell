
--the fundamental solution for Pell's equation can be found in the continued 
--fraction for sqrt(D)
--the minimal pair x and y satisfy x = a_i and y = b_i where a_i / b_i is a 
--term in the sequence of convergents of sqrt(23)

--so we're gonna reuse some code from p64 and p65 :)

import Data.List
import Useful

type FracPiece = (Integer, Integer, Integer, Integer)
type Frac = (Integer, Integer)
type ContFrac = [Integer]

fAdd :: Frac -> Frac -> Frac
fAdd (n1, d1) (0, _)   = (n1, d1)
fAdd (0, _)   (n2, d2) = (n2, d2)
fAdd (n1, d1) (n2, d2) = let dv = lcm d1 d2
                         in (n1 * (div dv d1) + n2 * (div dv d2), dv)

reciprocal :: Frac -> Frac
reciprocal (n, d) = (d, n)

fracStep :: FracPiece -> FracPiece
fracStep (i, r, b, d) = 
    let nd = div (r - b * b) d 
        nn = sqrt (fromIntegral r) + fromIntegral b
        nw = floor (nn / fromIntegral nd)
    in (nw, r, nd * nw - b, nd)

fracPieces :: Integer -> [FracPiece]
fracPieces n = 
    let w = floor (sqrt $ fromIntegral n)
        cf f = f : cf (fracStep f)
    in cf (w, n, w, 1)

continuedFrac :: Integer -> ContFrac
continuedFrac n = map (\(a, _, _, _) -> a) $ fracPieces n

nthConvergent :: Int -> Integer -> Frac
nthConvergent n x = 
    let fs = reverse $ take n (continuedFrac x)
    in foldl f (0, 0) fs
 where f res m = fAdd (m, 1) (reciprocal res) 

xyForD :: Integer -> Maybe (Integer, Integer)
xyForD d | elem d sqrs = Nothing
 where sqrs = takeWhile (<= 1000) (map (^2) [1..])
xyForD d = find f (map ((flip nthConvergent) d) [1..])
 where f (x, y) = x^2 - d * y^2 == 1

p66 :: Integer
p66 = maxingVal (\d -> fst (maybe (0, 0) id (xyForD d))) [1..1000] 