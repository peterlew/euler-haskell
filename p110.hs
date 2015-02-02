
import qualified Data.Numbers.Primes as P
import Useful

factExponents :: Integral a => a -> [Int]
factExponents x = 
    let rt = squareRoot x
        fe n (p:ps) = if p > x then if n > 1 then [1] else []
                 else let (tin, divN) = timesInto p n
                      in if tin > 0 then tin : fe divN ps
                         else fe divN ps
    in fe x P.primes
 where timesInto d y = if mod y d > 0 then (0, y) 
                       else let (nxt, np) = timesInto d (div y d)
                            in (nxt + 1, np)

choose :: Int -> [a] -> [[a]]
choose 0 xs = [[]]
choose n xs | n > length xs = [[]]
choose n xs | n == length xs = [xs]
choose n (x:xs) = map (x:) (choose (n - 1) xs) ++ choose n xs

numSolutionsExp :: [Int] -> Int
numSolutionsExp es = 
    let l = length es
        f n = 2^(l - n) * (sum $ map product (choose (l - n) es))
    in div ((sum $ map f [0..l]) + 1) 2

numSolutions :: Integer -> Int
numSolutions x = numSolutionsExp $ factExponents x

--candidates :: [[Int]]
--candidates = [ replicate s 5 ++ replicate t 4 ++ replicate w 3 ++ replicate x 2 ++ replicate y 1 | 
               --s <- [0..4], t <- [0..4], w <- [0..4], x <- [0..4], y <- [0..4] ]

cap :: Int
cap = 4000000

p110 :: [Int]
p110 = 
    let f bst bstNum [] = bst
        f bst bstNum (c:cs) = 
            let res = numSolutionsExp c
            in if res > cap then
                   let num = expToNum c
                   in if num < bstNum then 
                          f c num cs
                      else f bst bstNum cs
               else f bst bstNum cs
    in f [] 9100494818423000000 candidates

expToNum :: [Int] -> Integer
expToNum es = product $ zipWith (^) P.primes es

twoAddTo :: Int -> Int -> [[Int]]
twoAddTo n cap = map (\x -> [x, n - x]) $ reverse [(div (n + 1) 2)..(min cap (n - 2))]

twoAddUnder :: Int -> Int -> [[Int]]
twoAddUnder 4 cap = if cap >= 2 then [[2,2]] else [] 
twoAddUnder n cap | n < 4 = []
twoAddUnder n cap = twoAddTo n cap ++ twoAddUnder (n - 1) cap

descList :: Int -> Int -> [[Int]]
descList 1 left = map ((flip replicate) 1) [1..left]
descList cap left = 
    let f n = [n] : (map (n:) $ descList n (left - n))
    in concat $ map f [1..(min cap left)]

pwrOfTwo :: Int
pwrOfTwo = 60

candidates :: [[Int]]
candidates = [ hd ++ mid ++ back | 
               hd <- twoAddUnder pwrOfTwo (pwrOfTwo - 2), 
               mid <- twoAddUnder (div (pwrOfTwo - sum hd) 2) (hd!!1),
               back <- descList (mid!!1) (div (div (pwrOfTwo - sum hd) 2 - sum mid) 2)]