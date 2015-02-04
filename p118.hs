
import Data.List
import qualified Data.Numbers.Primes as P 
import Control.Monad.State
import qualified Data.Map as M 

isSubset :: String -> String -> Bool
isSubset s1 s2 = length (s2 \\ s1) == length s2 - length s1

buildPrimeSet :: String -> [Integer] -> [[Integer]]
buildPrimeSet ds (p:ps) = 
    let s = show p 
    in if length s > length ds then []
       else if sort s == ds then [[p]] ++ buildPrimeSet ds ps
            else if length s > div (length ds) 2 && ds == "123456789" then []
                 else if not $ isSubset s ds then buildPrimeSet ds ps
                      else map (p:) (buildPrimeSet (ds \\ s) ps) ++ buildPrimeSet ds ps

pandigitalPrimes :: [Integer]
pandigitalPrimes = 
    filter P.isPrime (map read (permutations "123456789"))