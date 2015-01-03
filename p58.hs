
import qualified Data.Numbers.Primes as P
import Data.List

p58 :: Int
p58 =
    let p58' side pCt dCt diff curNum = 
         let newNums = scanl (+) (curNum + diff) (replicate 3 diff)
             newpCt = pCt + (length $ filter P.isPrime newNums)
             newdCt = dCt + 4
             newCurNum = curNum + 4 * (side - 1)
         in if fromIntegral newpCt / fromIntegral newdCt < 0.1 then side
            else p58' (side + 2) newpCt newdCt (diff + 2) newCurNum
    in p58' 3 0 1 2 1