
import Data.List
import qualified Data.Numbers.Primes as P
import qualified Data.Map as M
import Useful

candidatePrimes :: [Int]
candidatePrimes = takeWhile (< 10000) P.primes

concatsWithToPrime :: Int -> Int -> Bool
concatsWithToPrime p1 p2 = 
    let s1 = show p1
        s2 = show p2
    in (P.isPrime $ read (s1 ++ s2)) && (P.isPrime $ read (s2 ++ s1))

addPrimeBuddiesToMap :: Int -> [Int] -> M.Map Int [Int] -> M.Map Int [Int] 
addPrimeBuddiesToMap n ns mp = 
    foldr f mp ns
 where f p m = if concatsWithToPrime n p 
                then M.insertWith (++) p [n] (M.insertWith (++) n [p] m)
               else m 

mInitial :: M.Map Int [Int]
mInitial = M.fromList $ zip candidatePrimes (repeat [])

primeBuddies :: M.Map Int [Int]
primeBuddies = 
    let pb mp [] = mp 
        pb mp (p:ps) = pb (addPrimeBuddiesToMap p ps mp) ps
    in pb mInitial candidatePrimes

lkOrEmpt :: Int -> M.Map Int [Int] -> [Int]
lkOrEmpt k mp = maybe [] id (M.lookup k mp)

groupsOf1 :: [[Int]]
groupsOf1 = map lst candidatePrimes

allBudsWithP :: Int -> [Int] -> Bool
allBudsWithP p bs = intersect bs (lkOrEmpt p primeBuddies) == bs

introduceP :: Int -> [[Int]] -> [[Int]]
introduceP p [] = []
introduceP p (b:bs) | p > head b = introduceP p bs
introduceP p (b:bs) = if allBudsWithP p b then (p:b) : introduceP p bs
                      else introduceP p bs

groupsOf2 :: [[Int]]
groupsOf2 = 
    let go2 [] = []
        go2 (p:ps) = introduceP p groupsOf1 ++ go2 ps
    in go2 candidatePrimes

groupsOf3 :: [[Int]]
groupsOf3 = 
    let go3 [] = []
        go3 (p:ps) = introduceP p groupsOf2 ++ go3 ps
    in go3 candidatePrimes

groupsOf4 :: [[Int]]
groupsOf4 = 
    let go4 [] = []
        go4 (p:ps) = introduceP p groupsOf3 ++ go4 ps
    in go4 candidatePrimes

groupsOf5 :: [[Int]]
groupsOf5 = 
    let go5 [] = []
        go5 (p:ps) = introduceP p groupsOf4 ++ go5 ps
    in go5 candidatePrimes
