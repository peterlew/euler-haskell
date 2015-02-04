
import Control.Monad.State
import qualified Data.IntMap as M 

waysToX :: Int -> State (M.IntMap Integer) Integer
waysToX 0 = return 1
waysToX x | x < 0 = return 0
waysToX x = do
    m <- get
    if M.member x m then return $ m M.! x
    else do
        heads <- sequence $ map (\n -> waysToX (x - n)) [1..4]
        let res = sum heads
        modify (M.insert x res)
        return res