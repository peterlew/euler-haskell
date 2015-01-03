
import Useful
import Control.Monad.State
import qualified Data.IntMap as M

endsAt :: Int -> State (M.IntMap Int) Int
endsAt n = do
    m <- get
    if M.member n m then return $ m M.! n
    else do
        res <- endsAt' n
        modify (M.insert n res)
        return res

endsAt' :: Int -> State (M.IntMap Int) Int
endsAt' n = if n == 1 || n == 89 then return n else endsAt (iter n)
 where iter x = sum $ map (^2) (map cToD $ show n)

upperN :: Int
upperN = 10000000

main' :: State (M.IntMap Int) Int
main' = 
    let mn a tot | a >= upperN = return tot
        mn a tot = do
            res <- endsAt a
            if res == 89 then mn (a + 1) (tot + 1)
            else mn (a + 1) tot
    in mn 1 0


