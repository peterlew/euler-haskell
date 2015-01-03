
import Data.List
import Data.Maybe

fourDigitFigurate :: Int -> [Int]
fourDigitFigurate n = dropWhile (< 1000) (takeWhile (< 10000) ns)
 where ns = map f [1..]
       f x = case n of 
              3 -> div (x * (x + 1)) 2
              4 -> x * x
              5 -> div (x * (3 * x - 1)) 2
              6 -> x * (2 * x - 1)
              7 -> div (x * (5 * x - 3)) 2
              8 -> x * (3 * x - 2)

chainFromCast :: String -> String -> [[Int]] -> Maybe [Int]
chainFromCast pre suf ls = 
    let cfc [] = Nothing
        cfc (p:ps) = let s = show p
                     in if take 2 s == pre then 
                         if length ls == 1 then 
                          if drop 2 s == suf then Just [p]
                          else cfc ps
                         else case chainFromCast (drop 2 s) suf (tail ls) of
                               Nothing -> cfc ps
                               Just res -> Just (p:res)
                        else cfc ps
    in cfc (ls!!0)

chainFromInit :: Int -> [[Int]] -> Maybe [Int]
chainFromInit p ls = case chainFromCast (drop 2 s) (take 2 s) ls of
                      Just res -> Just (p:res)
                      Nothing -> Nothing
 where s = show p

chainFromPermutation :: [Int] -> Maybe [Int]
chainFromPermutation ns = 
    let initPs = fourDigitFigurate $ ns!!0
        ls = map fourDigitFigurate (tail ns)
    in case find isJust (map ((flip chainFromInit) ls) initPs) of
        Just mi -> mi
        Nothing -> Nothing

allCycles :: [Maybe [Int]]
allCycles = filter isJust (map chainFromPermutation $ permutations [3..8])

p61 :: Int
p61 = sum $ maybe [] id (allCycles!!0)