import Data.Ratio
import Data.Array
import Data.Set hiding (map,elems)

import qualified Data.IntMap as M 
import qualified Data.Map as MM 
import Control.Monad.State
import Data.List
type Frac = (Integer, Integer)
data Circuit = Cap (Ratio Int) | Series Circuit Circuit | Parallel Circuit Circuit
  deriving (Eq, Show)

fReduce :: Frac -> Frac
fReduce (n, d) = let g = gcd n d in (div n g, div d g)

fAdd :: Frac -> Frac -> Frac
fAdd (n1, d1) (n2, d2) = fReduce (n1 * d2 + n2 * d1, d1 * d2)

fMult :: Frac -> Frac -> Frac
fMult (n1, d1) (n2, d2) = fReduce (n1 * n2, d1 * d2)

fInv :: Frac -> Frac
fInv (n, d) = (d, n)

allSums :: [Frac] -> [Frac] -> [Frac]
allSums l [] = l
allSums [] l = l
allSums l1 l2 = nub [fAdd a b | a <- l1, b <- l2]

allInvs :: [Frac] -> [Frac] -> [Frac]
allInvs l [] = l
allInvs [] l = l
allInvs l1 l2 = nub [fInv (fAdd (fInv a) (fInv b)) | a <- l1, b <- l2]

--this is a really good way to do it. It only takes, oh, TEN DAYS! :O

totalCaps :: Int -> State (M.IntMap [Frac]) [Frac]
totalCaps 0 = return []
totalCaps 1 = return [(60, 1)]
totalCaps n = do
    m <- get
    if M.member n m then return $ m M.! n
    else do
        r1 <- sequence $ map f [1..div n 2]
        let res = sort $ nub $ concat r1
        modify (M.insert n res)
        return res
 where f k = do
         r1 <- totalCaps k
         r2 <- totalCaps (n - k)
         return (allSums r1 r2 ++ allInvs r1 r2)

allUsing :: Int -> State (M.IntMap [Frac]) [Frac]
allUsing n = do
    r <- sequence $ map totalCaps [1..n]
    let res = sort $ nub $ concat r
    return res

lenUsing :: Int -> Int
lenUsing n = length (evalState (allUsing n) M.empty)

lensUsing :: Int -> State (M.IntMap [Frac]) [Int]
lensUsing n = do
    r <- sequence $ map allUsing [1..n]
    return $ map length r

main = do
    print $ evalState (lensUsing 18) M.empty

totalWays :: Int -> State (M.IntMap Int) Int
totalWays n | n < 3 = return n
totalWays n = do
    m <- get
    if M.member n m then return $ m M.! n
    else do
        r1 <- sequence $ map f [1..div n 2]
        r2 <- totalWays (n - 1)
        let res = sum r1 + r2
        modify (M.insert n res)
        return res
 where f k = do
        r1 <- totalWays k
        r2 <- totalWays (n - k)
        return (r1 * r2)

waysUsing :: Int -> State (M.IntMap Int) Int
waysUsing n = do
    r1 <- sequence $ map totalWays [1..n]
    return $ sum r1

evWays :: Int -> Int
evWays n = evalState (waysUsing n) M.empty

lsWithN :: Int -> [Frac]
lsWithN 0 = []
lsWithN 1 = [(1, 1)]
lsWithN n = nub (concat (map f [1..div n 2]) ++ map g (lsWithN (n - 1)))
 where f k = allSums (lsWithN k) (lsWithN (n - k))
       g k = fInv (fAdd (1, 1) (fInv k))

lsWithLTN :: Int -> [Frac]
lsWithLTN n = sort $ nub $ concat $ map lsWithN [1..n]

evalFrac :: Frac -> Double
evalFrac (n, d) = fromIntegral n / fromIntegral d

totalCapsGTNSeries :: Int -> Int -> State (MM.Map (Int, Int, Bool) [Frac]) [Frac]
totalCapsGTNSeries 0 _ = return []
totalCapsGTNSeries n gt | n < gt = return []
totalCapsGTNSeries 1 _ = return [(60, 1)]
totalCapsGTNSeries n gt = do
    m <- get
    if MM.member (n, gt, True) m then return $ m MM.! (n, gt, True)
    else do
        r1 <- sequence $ map f ([gt..div n 2]++[n])
        let res = sort $ nub $ concat r1
        modify (MM.insert (n, gt, True) res)
        return res
 where f k = do
         r1 <- totalCapsPar k
         r2 <- totalCapsGTNSeries (n - k) k 
         return $ allInvs r1 r2

totalCapsSeries :: Int -> State (MM.Map (Int, Int, Bool) [Frac]) [Frac]
totalCapsSeries n = totalCapsGTNSeries n 1

totalCapsGTNPar :: Int -> Int -> State (MM.Map (Int, Int, Bool) [Frac]) [Frac]
totalCapsGTNPar 0 _ = return []
totalCapsGTNPar n gt | n < gt = return []
totalCapsGTNPar 1 _ = return [(60, 1)]
totalCapsGTNPar 2 _ = return [(120, 1)]
totalCapsGTNPar n gt = do
    m <- get
    if MM.member (n, gt, False) m then return $ m MM.! (n, gt, False)
    else do
        r1 <- sequence $ map f [gt..div n 2]
        let res = sort $ nub $ concat r1
        modify (MM.insert (n, gt, False) res)
        return res
 where f k = do
         r1 <- totalCapsSeries k
         r2 <- totalCapsGTNSeries (n - k) k
         return $ allSums r1 r2

totalCapsPar :: Int -> State (MM.Map (Int, Int, Bool) [Frac]) [Frac]
totalCapsPar n = totalCapsGTNPar n 1

cs :: Int -> [Frac]
cs n = evalState (totalCapsSeries n) MM.empty
cp :: Int -> [Frac]
cp n = evalState (totalCapsPar n) MM.empty

allUsing2 :: Int -> State (MM.Map (Int, Int, Bool) [Frac]) [Frac]
allUsing2 n = do
    r <- sequence $ map totalCapsSeries [1..n]
    let res = sort $ nub $ concat r
    return res

lensUsing2 :: Int -> State (MM.Map (Int, Int, Bool) [Frac]) [Int]
lensUsing2 n = do
    r <- sequence $ map allUsing2 [1..n]
    return $ map length r

main2 = do
    print $ evalState (lensUsing2 18) MM.empty

combinePrev :: Int -> MM.Map Frac Int -> MM.Map Frac Int
combinePrev n mp = 
    let fs = MM.toList mp
    in Data.List.foldr f mp (concat 
       [[(fAdd f1 f2, c1 + c2), (fInv (fAdd (fInv f1) (fInv f2)), c1 + c2)] |
          (f1, c1) <- fs, (f2, c2) <- fs, c1 + c2 <= n])
 where f (frc, ct) = MM.insertWith min frc ct  

allUsing3 :: Int -> MM.Map Frac Int
allUsing3 1 = MM.fromList [((60, 1), 1)]
allUsing3 n = combinePrev n (allUsing3 (n - 1))

main3 = map MM.size (map allUsing3 [1..])

--Brute force with Fracs yields
--1, 3, 7, 15, 33, 
--70, 153, 341, 768, 1748, 
--4063, 9518, 22417, 53065, 126351,
--302205, 725736, 1748380 WRONG

--refined GTN algo (main2):
--1, 3, 7, 15, 33,
--72, 161, 369, 871, 2078,
--5015, 12202, 29837, 73399, 181533,
--450018, 1119018, 2788355 WRONG

--Better, still brute force, algo (main):
--1, 3, 7, 15, 35,
--77, 179, 429, 1039, 2525,
--6235, 15463, 38513, 96231, 241519,
--607339, 1529533, 3857447

--Corroborated by contructive algo (main3):
--1, 3, 7, 15, 35,
--77, 179, 429, 1039, 2525,
--6235, 15463, 38515, 96231, 241519,
--


--this is a solution from the euler thread. I'd like to write like this
--it only takes a few minutes

--it must be that using Sets, fromList, and toList is way faster
--than all the repeated appends and nubs

--using Ratio also helps a lot. I'm definitely gonna use that

--beautiful code

capacitance :: Circuit -> Ratio Int
capacitance (Cap n) = n
capacitance (Parallel c1 c2) = capacitance c1 + capacitance c2
capacitance (Series c1 c2) = 1 / (1/(capacitance c1) + 1/(capacitance c2))

n = 18
caps :: Array Int [Ratio Int]
caps = array (1,n) ((1,[1]) : [(i,cs) | i  <- [2..n],
         let cs = toList . fromList . map capacitance $ 
                  [ c | i' <- [1..(i `div` 2)], 
                        c1 <- caps!i', 
                        c2 <- caps!(i-i'), 
                        c <- [Parallel (Cap c1) (Cap c2), 
                              Series (Cap c1) (Cap c2)]] ])

main4 :: IO ()
main4 = putStrLn $ show . length . toList . fromList . concat $ elems caps