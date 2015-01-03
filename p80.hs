
import Useful
import Data.List.Split
import qualified Data.Scientific as S

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Integer -> Integer
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

numDig = 105

rtNToString :: Integer -> String
rtNToString n = let sc = S.scientific (squareRoot (n * 10^(2 * numDig))) (negate numDig)
                in S.formatScientific S.Fixed Nothing sc

frstHunDigs :: String -> String
frstHunDigs s = take 100 (foldl (++) "" (splitOn "." s))

nToRtHunDigSum :: Integer -> Int
nToRtHunDigSum n = sum $ map cToD (frstHunDigs $ rtNToString n)

p80 :: Int
p80 = foldl (+) 0 (map f [2..99])
 where f n | elem n (map (^2) [2..9]) = 0
       f n = nToRtHunDigSum n
