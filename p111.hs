
import Data.List
import qualified Data.Numbers.Primes as P 
import qualified Math.Combinatorics.Exact.Binomial as B

numDigits :: Int
numDigits = 10

numsWithNRepeated :: Int ->  Int -> [[Int]]
numsWithNRepeated n d = 
    let f x l | l == x = [replicate x d]
        f x 0 = []
        f x l = concat $ map (g x l) (if l == numDigits then [1..9] else [0..9])
        g x l c = map (c:) (f (x - if c == d then 1 else 0) (l - 1))
    in f n numDigits

dListToNum :: [Int] -> Integer
dListToNum ds =
    let f [] = 0
        f (x:xs) = fromIntegral x + 10 * f xs
    in f (reverse ds)

primesWithNRepeated :: Int -> Int -> [Integer]
primesWithNRepeated n d = 
    let cands = map dListToNum (numsWithNRepeated n d)
    in filter P.isPrime cands

sOfD :: Int -> Integer
sOfD d = 
    let f n = 
         let res = primesWithNRepeated n d
         in if res == [] then f (n - 1) else sum res
    in f numDigits

p111 :: Integer
p111 = sum $ map sOfD [0..9]