
import Data.List
import Data.List.Split
import Data.Maybe
import Useful (deleteIndex, demaybe)
import Control.Monad.State
import qualified Data.Map as M 

type Grid = [[Maybe Int]]
type NList = [[Int]]

vCount :: Int
vCount = 40

readCell :: String -> Maybe Int
readCell "-" = Nothing
readCell s = Just (read s)

gridToNList :: Grid -> NList
gridToNList g = map (findIndices isJust) g

testPathRec :: Int -> Int -> NList -> State (M.Map (Int, Int) Bool) Bool
testPathRec a b nl = do
    m <- get
    if M.member (a, b) m then return $ m M.! (a, b)
    else if elem b (nl!!a) || a == b then do
             modify (M.insert (a, b) True)
             return True
         else do
             modify (M.insert (a, b) False)
             ress <- mapM (\v -> testPathRec v b nl) (nl!!a)
             let res = any id ress
             modify (M.insert (a, b) res)
             return res

testPath :: Int -> Int -> NList -> State (M.Map (Int, Int) Bool) Bool
testPath a b nl = do
    res <- testPathRec a b nl
    if res then do
        modify (M.filter id)
        return res
    else return res

testAllPaths :: NList -> State (M.Map (Int, Int) Bool) Bool
testAllPaths nl = do
    ress <- sequence [ testPath a b nl | a <- [0..(vCount - 1)], b <- [a..(vCount - 1)] ]
    return $ all id ress

allPathsPreserved :: NList -> Bool
allPathsPreserved nl = evalState (testAllPaths nl) M.empty

locOf :: Int -> Grid -> (Int, Int)
locOf it g = 
    let lo a b | b == vCount = lo (a + 1) 0
        lo a b = if (g!!a)!!b == Just it then (a, b)
                 else lo a (b + 1)
    in lo 0 0

gDelete :: (Int, Int) -> Grid -> Grid
gDelete (row, col) g = 
    take row g ++ 
    [take col (g!!row) ++ [Nothing] ++ drop (col + 1) (g!!row)] ++
    drop (row + 1) g

removeVal :: Int -> Grid -> Grid
removeVal val g = 
    let (row, col) = locOf val g
    in gDelete (col, row) (gDelete (row, col) g)

gWeight :: Grid -> Int
gWeight g = div (sum $ map (sum . demaybe) g) 2

gStep :: Grid -> Grid
gStep g = 
    let tryRemove [] = g
        tryRemove (v:vs) = 
            let newG = removeVal v g
            in if allPathsPreserved $ gridToNList newG then newG
               else tryRemove vs
    in tryRemove (reverse $ sort $ concat $ map demaybe g)

reduceGrid :: Grid -> Grid
reduceGrid g = 
    let newG = gStep g
    in if g == newG then g else reduceGrid newG

main = do
    handle <- readFile "assets/p107.txt"
    let ls = lines handle
        lls = map (splitOn ",") ls
        grid = map (map readCell) lls
        reduced = reduceGrid grid
    print (gWeight grid - gWeight reduced)