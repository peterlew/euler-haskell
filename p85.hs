
import Useful

countInNxM :: Int -> Int -> Int
countInNxM n m = (div (n * (n + 1)) 2) * (div (m * (m + 1)) 2)

--a 820 x 1 rec contains a little over 2000000 recs, so cap at that

upperN :: Int
upperN = 820

allRecs :: [ (Int, Int) ]
allRecs = [ (n, m) | n <- [1..upperN], m <- [1..upperN], n <= m ]

p85 :: (Int, Int)
p85 = miningVal f allRecs
 where f (n, m) = abs ((countInNxM n m) - 2000000)