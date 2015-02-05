
import qualified Data.Numbers.Primes as P 
import Data.List
import Useful
import Control.Monad.State
import qualified Data.IntMap as M 

maxN :: Int
maxN = 100000

addMultsOfP :: Int -> State (M.IntMap [Int]) ()
addMultsOfP p = do
    m <- get
    _ <- sequence $ map (\n -> modify (M.insertWith (++) (p * n) [p])) [1..(div maxN p)]
    return ()

addAllP :: State (M.IntMap [Int]) (M.IntMap [Int])
addAllP = do
    _ <- sequence $ map addMultsOfP (takeWhile (<= maxN) P.primes)
    get

radMap :: M.IntMap Int
radMap = M.map product (evalState addAllP (M.fromList [(1,[1])]))

es = sort $ map (\(a, b) -> (b, a)) (M.toList radMap)