
import Useful
import Data.List
import Data.List.Split

type Point = (Int, Int)                       
data Sign = POS | NEG
    deriving (Show, Eq)

signOfYIntcpt :: Point -> Point -> Sign
signOfYIntcpt (x1, y1) (x2, y2) = 
    let slope = fromIntegral (y2 - y1) / fromIntegral (x2 - x1)
    in if fromIntegral y1 - fromIntegral x1 * slope > 0.0 then POS else NEG

quadrant :: Point -> Int
quadrant (x, y) = 
    case (x > 0, y > 0) of
        (True, True) -> 1
        (False, True) -> 2
        (False, False) -> 3
        (True, False) -> 0

signOfQuadrant :: Int -> Sign
signOfQuadrant n = 
    case n of
        0 -> NEG
        1 -> POS
        2 -> POS
        3 -> NEG

findOpposite :: [Int] -> Maybe [Int]
findOpposite ps | length ps == 2 = 
    if sort ps == [1, 3] || sort ps == [0, 2] then Just ps else Nothing
findOpposite (n:ps) = 
    let opp = mod (n + 2) 4
    in if elem opp ps then Just [n, opp] else findOpposite ps

containsOrigin :: [Point] -> Bool
containsOrigin ps = 
    let qs = map quadrant ps
    in case findOpposite qs of
        Nothing -> False
        Just opps -> let cq = head (qs \\ opps)
                         cInd = maybe (-1) id (elemIndex cq qs)
                         c = ps!!cInd
                     in if elem cq opps then
                         let aInd = maybe (-1) id (elemIndex (head (delete cq opps)) qs)
                             a = ps!!aInd
                             b = head (ps \\ [a, c])
                         in signOfYIntcpt a b /= signOfYIntcpt a c
                        else 
                         let [a, b] = delete c ps
                         in signOfQuadrant cq /= signOfYIntcpt a b

numsToPoints :: [Int] -> [Point]
numsToPoints [] = []
numsToPoints (n1:(n2:ns)) = (n1, n2) : numsToPoints ns

main = do
    handle <- readFile "assets/p102.txt"
    let ls = lines handle
        wds :: [[String]]
        wds = map (splitOn ",") ls
        nums :: [[Int]]
        nums = map (map read) wds
        points = map numsToPoints nums
        exPoint = points!!274
    print $ length points
    print $ exPoint
    print $ containsOrigin exPoint
    print $ length $ filter containsOrigin points
    return ()