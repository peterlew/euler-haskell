
import qualified Math.Combinatorics.Exact.Binomial as B 
import Control.Monad.State
import qualified Data.Map as M 

waysToXEndRed :: Int -> State (M.Map (Char, Int) Integer) Integer
waysToXEndRed 3 = return 1
waysToXEndRed x | x < 3 = return 0
waysToXEndRed x = do 
    m <- get
    if M.member ('R', x) m then return $ m M.! ('R', x)
    else do
        ress <- sequence $ map waysToXEndBlack [1..(x - 3)]
        let res = 1 + sum ress
        modify (M.insert ('R', x) res)
        return res

waysToXEndBlack :: Int -> State (M.Map (Char, Int) Integer) Integer
waysToXEndBlack 0 = return 0
waysToXEndBlack 1 = return 1
waysToXEndBlack x = do
    m <- get
    if M.member ('B', x) m then return $ m M.! ('B', x)
    else do
        res <- waysToX (x - 1)
        modify (M.insert ('B', x) res)
        return res

waysToX :: Int -> State (M.Map (Char, Int) Integer) Integer
waysToX x = do
    m <- get
    if M.member ('T', x) m then return $ m M.! ('T', x)
    else do
        r <- waysToXEndRed x 
        b <- waysToXEndBlack x
        let res = r + b
        modify (M.insert ('T', x) res)
        return res