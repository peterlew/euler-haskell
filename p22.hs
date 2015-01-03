import Data.List
import Data.List.Split

score :: [String] -> String -> Int
score names s = let s1 = foldl (+) 0 (map (\c -> fromEnum c - fromEnum 'A' + 1) s)
                    s2 = maybe 0 id (elemIndex s names) + 1
                in s1 * s2

main = do
    cnt <- readFile "assets/p22.txt"
    let ls = lines cnt
        l = ls!!0
        names = sort $ map (filter (/= '\"')) $ splitOn "," l
    return $ sum $ map fromIntegral $ map (score names) names

