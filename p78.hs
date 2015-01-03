
import Useful
import Data.List
import Control.Monad.State
import qualified Data.Map as M

type Partition = [Int]

pentagonals :: [Integer]
pentagonals = map f (weave [1..] (map negate [1..]))
 where f n = div (n * (3 * n - 1)) 2

numPartitions :: Integer -> State (M.Map Integer Integer) Integer
numPartitions 0 = return 1
numPartitions n = do
    m <- get
    if M.member n m then return (m M.! n)
    else do
        let toEval = takeWhile (>= 0) (map ((-) n) pentagonals)
        underPts <- sequence (map numPartitions toEval)
        let evs = zipWith (*) underPts (cycle [1, 1, -1, -1])
            tot = foldl (+) 0 evs
        modify (M.insert n tot)
        return tot

p78 :: State (M.Map Integer Integer) (Maybe Int)
p78 = do
    nps <- sequence $ map numPartitions [0..]
    return $ findIndex (\n -> mod n 1000000 == 0) nps
