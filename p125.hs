
import Data.List

cap :: Integer
cap = 10^8

isPalindrome :: Integer -> Bool
isPalindrome x = 
    let s = show x
    in s == reverse s

palStartingOn :: Integer -> [Integer]
palStartingOn n = 
    let f tot c = if tot > cap then []
                  else let newTot = tot + c^2
                       in if isPalindrome tot then tot : f newTot (c + 1)
                          else f newTot (c + 1)
    in f (n^2 + (n + 1)^2) (n + 2)

palAll :: [Integer]
palAll =
    let f c = if (c^2) > cap then []
              else palStartingOn c ++ f (c + 1)
    in f 1
