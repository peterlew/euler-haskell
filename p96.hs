
import Useful
import Data.List
import Data.Maybe

row :: Int -> [String] -> String
row n ss = ss!!n

col :: Int -> [String] -> String
col n ss = map (!!n) ss

box :: Int -> [String] -> String
box n ss = concat $ map (fs!!(mod n 3)) ((fs!!(div n 3)) ss)
 where fs = [take 3, drop 3 . take 6, drop 6 . take 9]

boxNum :: Int -> Int -> Int
boxNum row col = 3 * div row 3 + div col 3

possibleEntries :: (Int, Int) -> [String] -> String
possibleEntries (r, c) b = 
    let rw = row r b
        cl = col c b
        bx = box (boxNum r c) b
    in ['1'..'9'] \\ (nub $ concat [rw, cl, bx])

replaceEntry :: (Int, Int) -> Char -> [String] -> [String]
replaceEntry (i, j) c b = 
    let preRs = take i b
        postRs = drop (i + 1) b
        riq = b!!i
        preS = take j riq
        postS = drop (j + 1) riq
    in preRs ++ [preS ++ [c] ++ postS] ++ postRs

improvePosition :: (Int, Int) -> [String] -> [String]
improvePosition (i, j) b | (b!!i)!!j /= '0' = b
improvePosition (i, j) b =
    case possibleEntries (i, j) b of
          [v] -> replaceEntry (i, j) v b
          otherwise -> b

improveBoard :: [String] -> [String]
improveBoard b = foldr improvePosition b [ (i, j) | i <- [0..8], j <- [0..8] ]

improveAway :: [String] -> [String]
improveAway b = let nxtB = improveBoard b
                in if b == nxtB then nxtB
                   else improveAway nxtB

linesToBoards :: [String] -> [[String]]
linesToBoards ls | length ls == 10 = [drop 1 ls]
linesToBoards ls = drop 1 (take 10 ls) : linesToBoards (drop 10 ls)

isBoardComplete :: [String] -> Bool
isBoardComplete [] = True
isBoardComplete (r:rs) = if elem '0' r then False
                         else isBoardComplete rs

blankSpots :: [String] -> [(Int, Int)]
blankSpots b =
    let bs [] = []
        bs ((i,j):cs) = if (b!!i)!!j == '0' then (i, j) : bs cs
                        else bs cs
    in bs [ (i, j) | i <- [0..8], j <- [0..8] ]

spotWithFewOptions :: [String] -> (Int, Int)
spotWithFewOptions b = miningVal f (blankSpots b)
 where f spot = length $ possibleEntries spot b

resolveBoard :: [String] -> Maybe [String]
resolveBoard b' =
    let b = improveAway b'
    in if isBoardComplete b then Just b
       else let spot = spotWithFewOptions b
                options = possibleEntries spot b
            in if length options == 0 then Nothing
               else let rb [] = Nothing
                        rb (pos:poss) = case resolveBoard (replaceEntry spot pos b) of
                                         Nothing -> rb poss
                                         Just newB -> Just newB
                    in rb options

main = do
    handle <- readFile "assets/p96.txt"
    let ls = lines handle
        bs = linesToBoards ls
        bComps = map fromJust $ filter isJust $ map resolveBoard bs
        firstThrees = map (\ss -> take 3 (ss!!0)) bComps
    return $ sum (map read firstThrees)
