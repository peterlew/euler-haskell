
import Useful
import qualified Data.Map as M

--9! = 362880
--8 * 9! is only 7 digits, so we're looking at 7 digits or fewer
-- < 10 000 000

facMap :: M.Map Int Int
facMap = M.fromList $ zip [0..9] (map fac [0..9])

isSumOfFacDigits :: Int -> M.Map Int Int -> Bool
isSumOfFacDigits n mp = n == foldl (+) 0 (map f (show n))
 where f c = let dig = cToD c
             in maybe 0 id (M.lookup dig mp) 

p34 :: Int
p34 = 
 let p34' c tot = if c >= 10000000 then tot
                  else if isSumOfFacDigits c facMap then p34' (c + 1) (tot + c)
                       else p34' (c + 1) tot
 in p34' 3 0 