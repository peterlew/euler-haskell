
import Useful
import Data.List
import qualified Data.Map as M

addNsToMap :: [Int] -> M.Map Int ([Int], [Int]) -> M.Map Int ([Int], [Int])
addNsToMap [n1, n2, n3] m = M.adjust (appSnd [n2, n3]) n1
                             (M.adjust (appSnd [n3]) n2
                              (M.adjust (appFst [n1]) n2
                               (M.adjust (appFst [n1, n2]) n3 m)))
 where appFst nw (cur, v) = (nub (cur ++ nw), v)
       appSnd nw (v, cur) = (v, nub (cur ++ nw))

addNStrToMap :: String -> M.Map Int ([Int], [Int]) -> M.Map Int ([Int], [Int])
addNStrToMap s = addNsToMap (map cToD s)

mInit :: M.Map Int ([Int], [Int])
mInit = M.fromList $ zip [0,1,2,3,6,7,8,9] (zip (repeat []) (repeat []))

main = do
    handle <- readFile "assets/p79.txt"
    let ls = lines handle
        mp = foldr addNStrToMap mInit ls
    return mp

    