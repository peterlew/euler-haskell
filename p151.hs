
import Data.List
import Control.Monad.State
import qualified Data.Map as M 

expectedSingles :: [Int] -> State (M.Map [Int] Double) Double
expectedSingles [5] = return 0
expectedSingles [v] = do
    m <- get
    if M.member [v] m then return $ m M.! [v]
    else do 
        res' <- expectedSingles [(v + 1)..5]
        let res = res' + if v > 1 then 1.0 else 0.0
        modify (M.insert [v] res)
        return $ res
expectedSingles vs' = do
    let vs = sort vs'
    m <- get
    if M.member vs m then return $ m M.! vs
    else do
        let mult = 1.0 / fromIntegral (length vs)
            nextPos = map (f vs) vs
        ress <- sequence $ map expectedSingles nextPos
        let res = sum $ map (* mult) ress
        modify (M.insert vs res)
        return $ res
 where f vs 5 = delete 5 vs 
       f vs v = [(v + 1)..5] ++ delete v vs

expectedFromStart :: Int -> Double
expectedFromStart n = evalState (expectedSingles [(n + 1)..5]) M.empty
