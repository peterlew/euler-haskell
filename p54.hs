
import Data.List
import Data.List.Split
import Useful

getRank :: String -> Int
getRank ('T':_) = 10
getRank ('J':_) = 11
getRank ('Q':_) = 12
getRank ('K':_) = 13
getRank ('A':_) = 14
getRank (c:_) = cToD c

isFlush :: [Char] -> Bool
isFlush suits = all (== head suits) suits

isStraight :: [Int] -> Bool
isStraight [2,3,4,5,14] = True
isStraight ranks = [head ranks .. head ranks + 4] == ranks

rankHand :: [Int] -> [Char] -> Int
rankHand ranks suits = if isStraight ranks then
                        if isFlush suits then 37 * 15
                        else 18 * 15 + ranks!!0
                       else if isFlush suits then 19 * 15
                            else case sort $ map length (group ranks) of
                                  [1, 4] -> 36 * 15 + ranks!!2
                                  [2, 3] -> 20 * 15 + 15 * ranks!!2 + if ranks!!0 == ranks!!2 then ranks!!4 else ranks!!0
                                  [1, 1, 3] -> 17 * 15 + ranks!!2
                                  [1, 2, 2] -> 15 + 15 * ranks!!3 + ranks!!1
                                  [1, 1, 1, 2] -> mode ranks
                                  [1, 1, 1, 1, 1] -> 0

doesP1Win :: String -> Bool
doesP1Win s = 
    let s1 = splitOn " " $ take 14 s
        s2 = splitOn " " $ drop 15 s
        score1 = rankHand (ranks s1) (suits s1)
        score2 = rankHand (ranks s2) (suits s2)
    in score1 > score2 || (score1 == score2 && reverse (ranks s1) > reverse (ranks s2))
 where ranks = sort . (map getRank)
       suits = map last 

main = do
    handle <- readFile "assets/p54.txt"
    let ls = lines handle
    return $ length $ filter doesP1Win ls
