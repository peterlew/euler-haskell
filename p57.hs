
type Frac = (Integer, Integer)

simplify :: Frac -> Frac
simplify (num, den) = let g = gcd num den
                      in (div num g, div den g)

addFrac :: Frac -> Frac -> Frac
addFrac (n1, d1) (n2, d2) = simplify (n1 * d2 + n2 * d1, d1 * d2)

reciprocal :: Frac -> Frac
reciprocal (n, d) = (d, n)

nthIteration :: Int -> Frac
nthIteration n = addFrac (1, 1) (rc (n - 1))
 where rc 0 = (1, 2)
       rc x = reciprocal (addFrac (2, 1) (rc (x - 1)))

p57 :: Int
p57 = length $ filter f (map nthIteration [1..1000])
 where f (n, d) = digs n > digs d
       digs = length . show