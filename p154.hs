
import Useful (fac)
import Data.List
import qualified Data.IntMap as M 

cap :: Int
cap = 200000

nsInMFact :: Int -> Int -> Int
nsInMFact n m = sum $ map (\k -> div m (n^k)) [1..bigN]
 where bigN = let f e = if n^e > m then e - 1
                          else f (e + 1)
                in f 1

--apparently there are 199994 2s and 49998 5s in 200000! 
--we only need to leave 12 2s and 12 5s

--so the denominator i!j!k! can have up to 199982 2s and 49986 5s

max2s, max5s :: Int
max2s = nsInMFact 2 cap - 12
max5s = nsInMFact 5 cap - 12

nsMap :: Int -> M.IntMap Int
nsMap n = M.fromList $ zip [0..cap] (map (nsInMFact n) [0..cap])

twosMap, fivesMap :: M.IntMap Int
twosMap = nsMap 2
fivesMap = nsMap 5

numDens :: Integer
numDens = 
    sum [ fromIntegral $ div 6 (fac (4 - length (nub [i, j, k]))) |
          i <- [0..div cap 3],
          j <- [i..div (cap - i) 2],
          let k = cap - i - j,
          twosMap M.! i + twosMap M.! j + twosMap M.! k <= max2s,
          fivesMap M.! i + fivesMap M.! j + fivesMap M.! k <= max5s
        ]

main = do 
    print numDens