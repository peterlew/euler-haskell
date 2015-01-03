
import Data.List
import qualified Data.Numbers.Primes as P
import qualified Data.Map as M

upperN :: Integer
upperN = 10^7

candidatePrimes :: [Integer]
candidatePrimes = takeWhile (< upperN) P.primes

isPerm :: Integer -> Integer -> Bool
isPerm x y = ss x == ss y
 where ss = sort . show

p70 :: Integer
p70 = 
    let p70' _ bVal [] _ = bVal
        p70' bst bVal ps [] = p70' bst bVal (tail ps) candidatePrimes
        p70' bst bVal (p:ps) (q:qs) = let prod = p * q
                                          tot = (p - 1) * (q - 1)
                                      in if prod > upperN then p70' bst bVal ps candidatePrimes
                                         else if isPerm prod tot then
                                               let rat = fromIntegral prod / fromIntegral tot
                                               in if rat < bst then p70' rat prod (p:ps) qs
                                                  else p70' bst bVal (p:ps) qs
                                              else p70' bst bVal (p:ps) qs
    in p70' 3 6 candidatePrimes candidatePrimes