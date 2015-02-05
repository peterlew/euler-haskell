
import Data.List
import Control.Monad.State
import qualified Data.IntMap as M 
import qualified Data.Numbers.Primes as P

cap :: Int
cap = 200

cheatNums :: [(Int, Int, Int)]
cheatNums = nub $ sort $ [ (e, p, q) | e <- [2..7], 
                                         p <- pqCands, 
                                         q <- pqCands, 
                                         q <= p,
                                         2^e * p + q <= cap]
 where pqCands = drop 1 $ take 15 P.primes

cheatInts :: [Int]
cheatInts = map (\(a, b, c) -> 2^a * b + c) cheatNums

evalCNum :: (Int, Int, Int) -> State (M.IntMap Int) Int
evalCNum (e, p, q) = do
    mOfP <- mOfK p
    return $ mOfP + e + 1

mOfK :: Int -> State (M.IntMap Int) Int
mOfK x = do
    m <- get
    if M.member x m then return $ m M.! x
    else do
        nxt <- if mod x 2 == 0 then mOfK (div x 2)
                               else mOfK (x - 1)
        let nxtRes = nxt + 1
            cinds = elemIndices x cheatInts
        alts <- sequence $ map (\ind -> evalCNum (cheatNums!!ind)) cinds
        let res = if alts == [] then nxtRes else min nxtRes (minimum alts)
        modify (M.insert x res)
        return res

fillMap :: State (M.IntMap Int) (M.IntMap Int)
fillMap = do
    _ <- sequence $ map mOfK [1..cap]
    get

kMap :: M.IntMap Int
kMap = evalState fillMap (M.fromList [(1,0), (2,1)])

p122 :: Int
p122 = M.fold (+) 0 kMap

