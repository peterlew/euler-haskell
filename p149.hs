
import qualified Data.Map as M 
import Control.Monad.State

type Grid = M.Map Int Int
type Vals = M.Map (Int, Int) (Int, Int, Int)

cap :: Int
cap = 2000

sOfK :: Int -> State (Grid, Vals) Int
sOfK k = do
    (g, v) <- get
    if M.member k g then return $ g M.! k
    else do
        if k < 56 then do 
            let k' = fromIntegral k :: Integer
                res' = mod (100003 - 200003 * k' + 300007 * k'^3) (10^6) - 500000
                res = fromIntegral res' :: Int
            modify (gInsert k res)
            return res
        else do
            r1 <- sOfK (k - 24)
            r2 <- sOfK (k - 55)
            let res = mod (r1 + r2) (10^6) - 500000
            modify (gInsert k res)
            return res
 where gInsert k a (g, v) = (M.insert k a g, v)

bestFrom :: (Int, Int) -> State (Grid, Vals) (Int, Int, Int)
bestFrom (x, y) = do
    (g, v) <- get
    if M.member (x, y) v then return $ v M.! (x, y)
    else do
        let k = cap * y + x + 1
        val <- sOfK k
        (right, _, _) <- if x < cap - 1 then bestFrom (x + 1, y) else return (0, 0, 0)
        (_, down, _) <- if y < cap - 1 then bestFrom (x, y + 1) else return (0, 0, 0)
        (_, _, diag) <- if x < cap - 1 && y < cap - 1 then bestFrom (x + 1, y + 1) else return (0, 0, 0)
        let res = (max val (val + right), max val (val + down), max val (val + diag))
        modify (vInsert (x, y) res)
        modify (gDelete k)
        return res        
 where vInsert k a (g, v) = (g, M.insert k a v)
       gDelete k (g, v) = (M.delete k g, v)

findMax :: State (Grid, Vals) (IO ())
findMax = do
    res <- sequence $ map bestFrom cands
    let a = foldr myMax 0 res
    return $ print a
  where cands = [(x, y) | y <- reverse [0..cap - 1], x <- reverse [0..cap - 1]]
        myMax (a, b, c) cur = maximum [a, b, c, cur]

main = evalState findMax (M.empty, M.empty)

--for some reason (recursion?) evaluating this version of findMax 
--drains the memory while the new one (which I was at first using to try to 
--print progress as it ran) actually works, and pretty quickly. Leaving the 
--old one here for posterity

--deleteRow :: Int -> State (Grid, Vals) ()
--deleteRow row = modify vDelete
 --where vDelete (g, v) = (g, foldr f v [0..(cap - 1)])
       --f n m = M.delete (n, row) m
--
--findMax :: State (Grid, Vals) Int
--findMax = 
    --let f x y bst = 
         --if y < 0 then return bst
         --else if x < 0 then do
               --deleteRow (y + 1)
               --f (cap - 1) (y - 1) bst
              --else do
                --(rt, dn, dg) <- bestFrom (x, y)
                --f (x - 1) y (maximum [bst, rt, dn, dg])
    --in f (cap - 1) (cap - 1) 0

--main = do
    --print $ evalState findMax (M.empty, M.empty)