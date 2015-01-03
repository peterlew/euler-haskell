
import qualified Data.Map as M

findCollatz :: Integer -> M.Map Integer Integer -> (Integer, M.Map Integer Integer)
findCollatz n map = case M.lookup n map of 
                     Just val -> (val, map)
                     Nothing -> if mod n 2 == 0 then let (nxtVal, nxtMap) = findCollatz (div n 2) map
                                                     in (nxtVal + 1, M.insert n (nxtVal + 1) nxtMap)
                                else let (nxtVal, nxtMap) = findCollatz (3 * n + 1) map
                                     in (nxtVal + 1, M.insert n (nxtVal + 1) nxtMap)

p14 :: Integer
p14 = let p14' n best bestN map = if n >= 1000000 then bestN
                                  else let (val, nxtMap) = findCollatz n map
                                       in if val > best then p14' (n + 1) val n nxtMap
                                          else p14' (n + 1) best bestN nxtMap
      in p14' 1 0 0 (M.fromList [(1,1)])


