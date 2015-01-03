
import Useful
import Data.Char
import Data.List
import Data.List.Split

anagramsOf :: Ord a => [a] -> [[a]] -> [[a]]
anagramsOf w ws = let sw = sort w
                  in filter (f sw) ws
 where f y x = (sort x == y)

pairs :: [a] -> [(a, a)]
pairs ns | length ns < 2 = []
pairs (n:ns) = map (\x -> (n, x)) ns ++ map (\x -> (x, n)) ns ++ pairs ns

lstToAngrmPairs :: Ord a => [[a]] -> [([a], [a])]
lstToAngrmPairs ls =
    let angrmWds = map ((flip anagramsOf) ls) ls
        angrmWdGrps = filter (\lst -> length lst > 1) angrmWds
    in nub $ concat $ map pairs angrmWdGrps

nDigitSquareAnagrams :: Int -> [(String, String)]
nDigitSquareAnagrams n =
    let sqrs = dropWhile (< 10^(n-1)) $ takeWhile (< 10^n) (map (^2) [1..])
        sqrWds = map show sqrs
        sqrPairs = lstToAngrmPairs sqrWds
    in sqrPairs

stringToPattern :: String -> String
stringToPattern s =
    let stp i [] = []
        stp i (c:cs) = if isAlphaNum c then 
                        let sp = spChars!!i
                        in sp : stp (i + 1) (replace [c] [sp] cs)
                       else c : stp i cs
    in stp 0 s
 where spChars = "!@#$%^&*()"

stringsToPerm :: (String, String) -> [Int]
stringsToPerm (s1', s2') =
    let stp [] s2 perm = perm
        stp (s:ss) s2 perm = 
            let inds = elemIndices s s2
                go = head (inds \\ perm)
            in stp ss s2 (perm ++ [go])
    in stp s1' s2' []

matchStringsToSquares :: (String, String) -> [(String, String)]
matchStringsToSquares (s1, s2) | length s1 > 7 = []
matchStringsToSquares (s1, s2) = filter f (nDigitSquareAnagrams (length s1))
 where f (sq1, sq2) = stringToPattern sq1 == stringToPattern s1 &&
                      stringsToPerm (s1, s2) == stringsToPerm (sq1, sq2)

main = do
    handle <- readFile "assets/p98.txt"
    let ls = lines handle
        wds = map (filter (/= '\"')) (splitOn "," (ls!!0))
        aPrs = lstToAngrmPairs wds
        matches = concat $ map matchStringsToSquares aPrs
    return $ matches