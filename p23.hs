
import Useful
import Data.List
import qualified Data.Map as M

abundants :: M.Map Int Bool
abundants = let ab c mp = if c > 28123 then mp
                          else if isAbundant c then ab (c + 1) (M.insert c True mp)
                               else ab (c + 1) mp
            in ab 1 M.empty
 where isAbundant n = sumOfDivisors n > n

notAbundantSum :: Int -> Bool
notAbundantSum n = let abundantsList = M.keys abundants
                       nas n [] = True
                       nas n (a:as) = if a >= n then True
                                      else if isAbundant (n - a) then False
                                           else nas n as
                   in nas n abundantsList
 where isAbundant n = case M.lookup n abundants of 
                       Just True -> True
                       Nothing -> False


