
import qualified Math.Combinatorics.Exact.Binomial as B 
import qualified Data.Numbers.Primes as P
import Useful
import Control.Monad.State
import qualified Data.IntMap as M 
import Data.List
import Prelude hiding (exp)

--I WENT ON SUCH A LONG AND FASCINATING WILD GOOSE CHASE, I'M LEAVING
--THE MESS HERE FOR THE SAKE OF INTEREST. THE REAL SOLUTION IS AFTER THE
--DOTTED LINE

numDivisors :: Integral a => a -> Int 
numDivisors x' = 
    let nf x p ct = if p > div x 2 then if x > 1 then ct * 2 else ct
                    else let din = dInto p x 
                         in nf (div x (p^din)) (p + 1) (ct * (dInto p x + 1))
    in nf x' 2 1
 where dInto d n = if mod n d > 0 then 0 else 1 + dInto d (div n d)

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

numUniqueKMN :: Integral a => a -> Int
numUniqueKMN n = 
    let exps = factExponents n
        nonUnique = div (product $ map f exps) (2^(length exps)) 
    in div (nonUnique + (product $ map g exps)) 2
 where f x = x^2 + 3*x + 2
       g x = div x 2 + 1

numSquareContainingDivisors :: Integral a => a -> Int 
numSquareContainingDivisors x = 
    numSquaresFromExp $ (map (+ 1) $ factExponents x)

numSquaresFromExp :: [Int] -> Int
numSquaresFromExp es =
    let f [] _ = 0
        f (n:ns) prod = if n > 2 then 
                         let diff = 2 * (div prod n)
                         in prod - diff + f ns diff
                        else f ns prod
    in f es (product es)

allFactorsExp :: [Int] -> [[Int]]
allFactorsExp [] = [[]]
allFactorsExp (e:es) = concat $ map (\x -> map (x:) (allFactorsExp es)) [0..e]

numSquaresExp :: [Int] -> Int 
numSquaresExp es = length $ filter hasSquare (allFactorsExp es)
 where hasSquare xs = any (> 1) xs

--numSquarePairsExp :: [Int] -> Int
--numSquarePairsExp es =
    --let f ind = 
         --if ind == length es then 0
         --else if es!!ind > 2 then (product $ inflateTwos $ deleteIndex ind es) + f (ind + 1)
              --else f (ind + 1)
    --in f 0
 --where inflateTwos xs = map (\n -> n + max 0 (n - 2)) xs

numSquarePairsExp :: [Int] -> Int
numSquarePairsExp es =
    let sqrs = filter (any (> 1)) $ allFactorsExp es
    in length $ filter (lt es) [ fAdd f1 f2 | f1 <- sqrs, f2 <- sqrs ]
 where fAdd = zipWith (+)
       lt g l = all (>= 0) $ zipWith (-) g l

numSquarePairs :: Integral a => a -> Int 
numSquarePairs n = numSquarePairsExp $ factExponents n

numSquares :: Integral a => a -> Int
numSquares n = numSquaresExp $ factExponents n

exp :: Integral a => a -> [Int]
exp x = map (+ 1) $ factExponents x

--numSquareContainingDivisorPairs :: Integral a => a -> Int
--numSquareContainingDivisorPairs x =
    --let exps = map (+ 1) $ factExponents x
        --ns = numSquaresFromExp exps
        --pos = ns * (ns - 1)
        --cands = takeWhile (> 2) $ reverse $ sort exps
        --rest = product $ dropWhile (> 2) $ reverse $ sort exps

--numSolutions :: Integral a => a -> Int
--numSolutions n = numUniqueKMN n - numSquares n - numSquarePairs n

waysToSplitMN :: Integral a => a -> Int
waysToSplitMN x = div (numDivisors x + 1) 2


primeFactors :: Integral a => a -> [a]
primeFactors x =
    let pf n c = if c > div n 2 then if n > 1 then [n] else []
                 else if mod n c == 0 then c : pf (div n c) c
                      else pf n (c + 1)
    in pf x 2

numSquareMultiplesExp :: [Int] -> Int
numSquareMultiplesExp es = 
    let prod = product es
        f [] = 0
        f (n:ns) = if n > 2 then prod - (div prod n) + f ns
                   else f ns
    in f es

eAdd :: [Int] -> [Int] -> [Int]
eAdd = zipWith (+)

eComp :: [Int] -> [Int] -> Bool
eComp es1 es2 = all id $ zipWith (==) es1 es2

allDivisors :: Integral a => a -> [a]
allDivisors x = 
    let f n c = if c > div n 2 then [n]
                else if mod n c == 0 then c : f n (c + 1)
                     else f n (c + 1)
    in f x 1

tri :: Int -> Int 
tri n = div (n * (n + 1)) 2

sqrs :: [Int]
sqrs = map (^2) [2..]   

hasSquare :: Int -> Bool
hasSquare x = 
    let f (n:ns) = if mod x n == 0 then True
                   else if n > x then False
                        else f ns
    in f sqrs

--numSolutions :: Int -> Int
--numSolutions n = 
    --let divs = allDivisors n
        --l = length divs
        --tot = tri l
        --diff _ [] = 0
        --diff ind (d:ds) = if hasSquare d then ind - l + diff (ind + 1) ds
                          --else diff (ind + 1) ds
    --in div (tot + diff 0 divs + 1) 2

waysToSplitMNCoprime :: Integer -> Int
waysToSplitMNCoprime n = 
    let rt = squareRoot n
        f c = if c > rt then 0
              else if mod n c == 0 then 
                       if gcd c (div n c) == 1 then 1 + f (c + 1)
                       else f (c + 1)
                   else f (c + 1)
    in f 1

--numSolutions :: Integer -> Int
--numSolutions n = 
    --let f c = if c > n then 0
              --else if mod n c == 0 then 
                       --waysToSplitMNCoprime (div n c) + f (c + 1)
                   --else f (c + 1)
    --in f 1

waysToSplitMNCoprimeExp :: [Int] -> Int
waysToSplitMNCoprimeExp es = 2^(length es)

removeK :: [Int] -> [Int] -> [Int]
removeK = zipWith (-)

-----------------------------------------------------------------------------

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