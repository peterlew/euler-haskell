
import Data.List

isIntSol :: Int -> Int -> Int -> Bool
isIntSol a b c = let undr = a^2 + (b + c)^2
                     undrRt = sqrt $ fromIntegral undr
                 in undrRt == fromIntegral (floor undrRt)

isSqr :: Int -> Bool
isSqr n = sq * sq == n
 where sq = floor $ sqrt $ fromIntegral n

sqrs :: [Int]
sqrs = map (^2) [1..]

sqrsBtwn :: Int -> Int -> [Int]
sqrsBtwn a b = takeWhile (<= b) $ dropWhile (<= a) sqrs

posBCSqrForM :: Int -> [Int]
posBCSqrForM m = filter isSqr (map (+ (negate mSqr)) (sqrsBtwn mSqr (5*mSqr)))
 where mSqr = m^2

waysToXLTEN :: Int -> Int -> Int
waysToXLTEN x n = if n >= x - 1 then div x 2
                  else max (n - div x 2 + if mod x 2 == 0 then 1 else 0) 0 

numSolsForM :: Int -> Int
numSolsForM m = sum $ map f (posBCSqrForM m)
 where f sol = waysToXLTEN (floor $ sqrt $ fromIntegral sol) m

totalSolsForM :: Int -> Int
totalSolsForM m = sum (map numSolsForM [1..m])

p86 :: Maybe Int
p86 = findIndex (\m -> totalSolsForM m > 1000000) [1800..]