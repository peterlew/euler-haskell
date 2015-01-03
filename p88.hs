
import Data.List
import Control.Monad.State
import qualified Data.Map as M

upperD, upperN :: Int
upperD = 12000
upperN = 2 * upperD

waysToProdN :: Int -> State (M.Map (Int, Int) [[Int]]) [[Int]]
waysToProdN n = wtp n (div n 2)
 where wtp n c = do
       m <- get
       if M.member (n, c) m then return (m M.! (n, c))
       else if c == 1 then return []
            else if n == 1 then return [[]]
                 else if mod n c == 0 then do
                       nxt <- wtp (div n c) c
                       rest <- wtp n (c - 1)
                       let res = map (c:) nxt ++ rest
                       modify (M.insert (n, c) res)
                       return res
                      else wtp n (c - 1)

allWaysToProd :: State (M.Map (Int, Int) [[Int]]) (M.Map Int [[Int]])
allWaysToProd = foldM f M.empty [2..upperN]
 where f res n = do
       wys <- waysToProdN n
       return $ M.insert n wys res

fillDMap :: M.Map Int Int
fillDMap = M.foldlWithKey f M.empty (M.map g (evalState allWaysToProd M.empty))
g [] = []
g (l:ls) = (length l + product l - sum l) : g ls
f res n [] = res
f res n (b:bs) = if M.member b res then f res n bs
                 else f (M.insert b n res) n bs

allNs :: [Int]
allNs = nub $ M.elems (M.filterWithKey (\k _ -> k <= upperD) fillDMap)

