
import qualified Data.Map as M

allCoins :: [Int]
allCoins = [1, 2, 5, 10, 20, 50, 100, 200]

waysToMakeWith :: Int -> [Int] -> Int
waysToMakeWith n _ | n < 0 = 0
waysToMakeWith 0 _ = 1
waysToMakeWith n coins = let wtm n mp = case M.lookup n mp of 
                                         Just val -> val
                                         Nothing -> wtm n (foldl (uMapAmt n coins) (M.insert n 0 mp) coins)
                         in wtm n M.empty
 where uMapAmt n cns mp amt = let uAmt = n - amt
                                  uCns = takeWhile (<= amt) cns
                              in case M.lookup uAmt mp of
                                  Nothing -> uMapAmt n cns (M.insert uAmt (waysToMakeWith uAmt uCns) mp) amt
                                  Just val -> M.adjust (+ val) n mp

waysToMake :: Int -> Int
waysToMake = (flip waysToMakeWith) allCoins