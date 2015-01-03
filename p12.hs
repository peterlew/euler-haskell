
import qualified Data.IntMap as M

primeFactorization :: Int -> M.IntMap Int
primeFactorization n = let pf n p mp = if p > floor (sqrt (fromIntegral n)) then M.insertWith (+) n 1 mp
                                       else if mod n p == 0 then pf (div n p) p (M.insertWith (+) p 1 mp)
                                            else pf n (p + 1) mp
                       in pf n 2 M.empty

numberOfFactors :: Int -> Int
numberOfFactors 1 = 1
numberOfFactors n = M.foldl (\a b -> a * (b + 1)) 1 (primeFactorization n)

p12 :: Int
p12 = let p12' n = let tri = div (n * (n + 1)) 2
                       nof = numberOfFactors tri
                   in if nof > 500 then tri
                      else p12' (n + 1)
      in p12' 1
