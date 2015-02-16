
import Control.Monad.State
import qualified Data.Map as M 

--not sure how much extra you need to assure finding all solutions
--1.5 x what you're going to look at seems fairly safe
maxN :: Integer
maxN = 1500000

maxA :: Integer
maxA = div (maxN + 1) 4

markForAX :: Integer -> Integer -> State (M.Map Integer Int) ()
markForAX a x = modify (M.insertWith (+) (3 * a^2 + 2 * a * x - x^2) 1)

markForA :: Integer -> State (M.Map Integer Int) [()]
markForA a = let lwr = if maxN <= 4 * a^2 then 
                        a + (ceiling $ sqrt $ fromIntegral (4 * a^2 - maxN))
                       else 1
             in sequence $ map (markForAX a) [lwr..(3 * a - 1)]

markAll :: State (M.Map Integer Int) (M.Map Integer Int)
markAll = do
    _ <- sequence $ map markForA [1..maxA]
    get

solMap :: M.Map Integer Int
solMap = evalState markAll M.empty
