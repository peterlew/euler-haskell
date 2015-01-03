
import Data.List.Split
import Control.Monad.State
import qualified Data.Map as M

bestFrom :: Int -> Int -> [[Int]] -> State (M.Map (Int, Int) Int) Int
bestFrom 79 79 grid = return $ (grid!!79)!!79
bestFrom i j grid = do
    m <- get
    if M.member (i, j) m then return (m M.! (i, j))
    else do
        nxt <- if i == 79 then bestFrom i (j + 1) grid
               else if j == 79 then bestFrom (i + 1) j grid 
                    else liftM2 min (bestFrom i (j + 1) grid) (bestFrom (i + 1) j grid)
        let tot = nxt + (grid!!i)!!j
        modify (M.insert (i, j) tot)
        return tot

main = do
    handle <- readFile "assets/p81.txt"
    let ls = lines handle
        grid = map (map read) (map (splitOn ",") ls)
        res = evalState (bestFrom 0 0 grid) M.empty
    return res