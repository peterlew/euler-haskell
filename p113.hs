
import Control.Monad.State
import qualified Data.Map as M 

maxDigs :: Int
maxDigs = 100

numWaysUnderN :: Int -> Int -> State (M.Map (Int, Int) Integer) Integer
numWaysUnderN n 0 = return 1
numWaysUnderN n len = do
    m <- get
    if M.member (n, len) m then return $ m M.! (n, len)
    else do
        res <- sequence $ map (\x -> numWaysUnderN x (len - 1)) [0..n]
        let tot = 1 + sum res
        modify (M.insert (n, len) tot)
        return tot

numWaysOverN :: Int -> Int -> State (M.Map (Int, Int) Integer) Integer
numWaysOverN n 0 = return 1
numWaysOverN n len = do
    m <- get
    if M.member (n, len) m then return $ m M.! (n, len)
    else do
        res <- sequence $ map (\x -> numWaysOverN x (len - 1)) [n..9]
        let tot = 1 + sum res
        modify (M.insert (n, len) tot)
        return tot

numWaysUnder :: State (M.Map (Int, Int) Integer) Integer
numWaysUnder = do
    res <- sequence $ map (\x -> numWaysUnderN x (maxDigs - 1)) [1..9]
    return $ sum res

numWaysOver :: State (M.Map (Int, Int) Integer) Integer
numWaysOver = do
    res <- sequence $ map (\x -> numWaysOverN x (maxDigs - 1)) [1..9]
    return $ sum res

numConstant :: Int
numConstant = 9 * maxDigs

numNonBouncy :: Integer
numNonBouncy = (evalState numWaysOver M.empty) + 
               (evalState numWaysUnder M.empty) -
               fromIntegral numConstant