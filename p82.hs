
import Data.List.Split 
import Control.Monad.State
import qualified Data.Map as M

data Dir = UP | DOWN | RIGHT
 deriving (Show, Eq, Ord)

bestFrom :: Int -> Int -> Dir -> [[Int]] -> State (M.Map (Int, Int, Dir) Int) Int
bestFrom i 79 _ grid = return $ (grid!!i)!!79
bestFrom i j dir grid = do
    m <- get
    if M.member (i, j, dir) m then return (m M.! (i, j, dir))
    else do
        let pos = case (i, dir) of 
                   (0, UP) -> [right]
                   (0, RIGHT) -> [right, down]
                   (79, DOWN) -> [right]
                   (79, RIGHT) -> [right, up]
                   (_, UP) -> [right, up]
                   (_, DOWN) -> [right, down]
                   (_, RIGHT) -> [right, up, down]
        nxt <- sequence pos
        let tot = minimum nxt + (grid!!i)!!j
        modify (M.insert (i, j, dir) tot)
        return tot
 where down  = bestFrom (i + 1) j DOWN grid 
       up    = bestFrom (i - 1) j UP grid 
       right = bestFrom i (j + 1) RIGHT grid 

minPath :: [[Int]] -> State (M.Map (Int, Int, Dir) Int) Int
minPath g = do
    ps <- sequence $ map f [0..79]
    return $ minimum ps
 where f n = bestFrom n 0 RIGHT g

main = do
    handle <- readFile "assets/p82.txt"
    let ls = lines handle
        grid = map (map read) (map (splitOn ",") ls)
    return $ evalState (minPath grid) M.empty 
