
import Useful
import Data.List.Split

triangleNumbers :: [Int]
triangleNumbers = map f [1..]
 where f n = div (n * (n + 1)) 2

wordValue :: String -> Int
wordValue s = foldr (+) 0 (map capLToD s)

viableTriangles :: [Int]
viableTriangles = takeWhile (< 193) triangleNumbers

isTriangleWord :: String -> Bool
isTriangleWord s = elem (wordValue s) viableTriangles

main = do
    handle <- readFile "assets/p42.txt"
    let quotedWords = splitOn "," $ (lines handle)!!0
        words = map (filter (/= '\"')) quotedWords
    return $ length $ filter isTriangleWord words