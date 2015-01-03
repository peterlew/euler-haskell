
import Useful
import qualified Data.Map as M

allRotations :: Eq a => [a] -> [[a]]
allRotations ls = 
    let ar ls cur cum = if ls == cur then cum
                        else ar ls (rotate cur) (cur:cum)
    in ar ls (rotate ls) [ls]
 where rotate xs = tail xs ++ [head xs]

allNumRotations :: Int -> [Int]
allNumRotations n = let nums = map read (allRotations $ show n) :: [Int]
                        words = map show nums
                        filtered = filter f words
                    in map read filtered
 where f s = length s == length (show n)

isCircularPrime :: Int -> Bool
isCircularPrime 2 = True
isCircularPrime n = 
    let digs = map cToD (show n)
    in if any (\x -> mod x 2 == 0) digs then False
       else all isPrime $ allNumRotations n

allRotPrimeMap :: Int -> M.Map Int Bool
allRotPrimeMap cap = 
    let arpm n mp = if n >= cap then mp 
                    else case M.lookup n mp of
                          Just b -> arpm (n + 1) mp
                          Nothing -> let b = isCircularPrime n
                                     in arpm (n + 1) (markAllRotations n b mp)
    in arpm 2 M.empty

markAllRotations :: Int -> Bool -> M.Map Int Bool -> M.Map Int Bool
markAllRotations n b mp =
    let rots = allNumRotations n
        mar [] m = m
        mar (x:xs) m = mar xs (M.insert x b m)
    in mar rots mp

