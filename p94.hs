
import Useful

upperP :: Integer
upperP = 1000000000

upperN :: Integer
upperN = squareRoot $ div upperP 12

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

isSquare :: Integer -> Bool
isSquare n = (squareRoot n)^2 == n

contribForN1 :: Integer -> Integer
contribForN1 n =
    let mSqr = 3 * n^2 + 1
    in if isSquare mSqr then 3 * (mSqr + n^2) + 1
       else 0

contribForN2 :: Integer -> Integer
contribForN2 n =
    let posSqr = 3 * n^2 + 1
    in if isSquare posSqr then
        let m = 2 * n + squareRoot posSqr
        in 3 * (m^2 + n^2) - 1
    else 0

allContribs :: [Integer]
allContribs = filter (< upperP) $ concat $ map f [contribForN1, contribForN2]
 where f func = filter (> 0) $ map func [1..upperN]

