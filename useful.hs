
module Useful where

import Data.List (elemIndex, maximum, sort, group, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isJust, fromJust)

fac :: Integral a => a -> a
fac n | n < 0 = undefined
fac 0 = 1
fac n = n * fac (n - 1)

sumOfDivisors :: Int -> Int
sumOfDivisors n = let stop = floor $ sqrt $ fromIntegral n
                      sod n c tot = if c > stop then tot
                                    else if mod n c == 0 then 
                                          if c * c == n then sod n (c + 1) (tot + c)
                                          else sod n (c + 1) (tot + c + div n c)
                                         else sod n (c + 1) tot
                  in sod n 2 0 + 1
 
isPrime :: Integral a => a -> Bool
isPrime n | n < 2 = False
isPrime n = let stop = floor $ sqrt $ fromIntegral n
                ip n c = if c > stop then True
                         else if mod n c == 0 then False 
                              else ip n (c + 1)
            in ip n 2

fibs :: [Integer]
fibs = [1] ++ zipWith (+) fibs (0:fibs)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple f (a1, a2, a3) = (f a1, f a2, f a3)

foldrTriple :: (a -> b -> a) -> a -> (b, b, b) -> a
foldrTriple f res (b1, b2, b3) = f (f (f res b1) b2) b3

cToD :: Char -> Int
cToD c = fromEnum c - fromEnum '0'

capLToD :: Char -> Int
capLToD c = fromEnum c - fromEnum 'A' + 1

posIntQuadRoot :: Integral a => a -> a -> a -> Maybe a
posIntQuadRoot a b c = 
    let x = div ((-1) * b + floor (sqrt (fromIntegral (b^2 - 4 * a * c)))) (2 * a)
    in if a * x^2 + b * x + c == 0 then Just x
       else Nothing

hasPosIntQuadRoot :: Integral a => a -> a -> a -> Bool
hasPosIntQuadRoot a b c = case posIntQuadRoot a b c of 
                           Just _ -> True
                           Nothing -> False

maxingIndex :: Ord b => (a -> b) -> [a] -> Int
maxingIndex f ls = let newLs = map f ls
                   in maybe 0 id (elemIndex (maximum newLs) newLs)

maxingVal :: Ord b => (a -> b) -> [a] -> a
maxingVal f ls = let newLs = map f ls
                 in ls!!(maybe 0 id (elemIndex (maximum newLs) newLs))

miningVal :: Ord b => (a -> b) -> [a] -> a
miningVal f ls = let newLs = map f ls
                 in ls!!(maybe 0 id (elemIndex (minimum newLs) newLs))

mode :: Ord a => [a] -> a
mode xs = (maxingVal length (group $ sort xs))!!0

lst :: a -> [a]
lst x = [x]

elemIndexOrd :: Ord a => a -> [a] -> Maybe Int
elemIndexOrd a' ls' =
    let ei a [] _ = Nothing
        ei a (l:ls) ct 
         | a == l = Just ct
         | a < l  = Nothing
         | otherwise = ei a ls (ct + 1)
    in ei a' ls' 0

weave :: [a] -> [a] -> [a]
weave ls [] = ls
weave [] ls = ls
weave (a:as) (b:bs) = a : b : weave as bs

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to xs = intercalate to (splitOn from xs)

isInt :: RealFrac a => a -> Bool
isInt x = x == fromIntegral (round x)

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
squareRoot :: Integral a => a -> a
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters

isSquare :: Integral a => a -> Bool
isSquare n = (squareRoot n)^2 == n

leave :: Int -> [a] -> [a]
leave n xs = drop (length xs - n) xs

deleteIndex :: Int -> [a] -> [a]
deleteIndex ind ls = take ind ls ++ drop (ind + 1) ls 

demaybe :: [Maybe a] -> [a]
demaybe ms = map fromJust $ filter isJust ms

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

fmod :: Double -> Double -> Double
fmod x n | x >= 0 && x < n = x
fmod x n | x < 0 = fmod (x + n) n
fmod x n = fmod (x - n) n