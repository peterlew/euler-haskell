
import Useful 
import Data.List
--say x = d^e and the sum of the digits in x is d

maxDigs :: Int
maxDigs = 16

maxX :: Integer
maxX = 10^maxDigs

sumOfDigs :: Integer -> Int
sumOfDigs x = sum $ map cToD (show x)

xsForD :: Int -> [Integer]
xsForD d =
    let dIntgr = fromIntegral d
        f posX = if posX > maxX then []
                 else if sumOfDigs posX == d then posX : f (dIntgr * posX)
                      else f (dIntgr * posX)
    in f (dIntgr * dIntgr)

allXs :: [Integer]
allXs = sort $ nub $ concat $ map xsForD [2..(9 * maxDigs)]

