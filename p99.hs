
import Useful
import Data.List.Split

findBest :: [[Integer]] -> Int
findBest pairs = 
    let fb [] _ bst _ = bst
        fb ([b,e]:prs) bstVal bst cur = if b^e > bstVal then fb prs (b^e) cur (cur + 1) 
                                        else fb prs bstVal bst (cur + 1)
    in fb pairs 0 0 0

main :: IO Int
main = do
    handle <- readFile "assets/p99.txt"
    let ls = lines handle
        strPairs = map (splitOn ",") ls
        numPairs = map (map read) strPairs
    return $ findBest numPairs
