import qualified Data.IntMap as M

primeFactorExp :: Int -> Int -> Int
primeFactorExp n p = if mod n p == 0 then primeFactorExp (div n p) p + 1
                     else 0

updatePrimesMap :: Int -> M.IntMap Int -> M.IntMap Int
updatePrimesMap n map = let upm n c mp = let pfe = primeFactorExp n c
                                             newn = div n (c^pfe)
                                             newmp = M.insertWith max c pfe mp
                                         in if c > div newn 2 then M.insertWith max newn 1 newmp
                                            else upm newn (c+1) newmp
                        in upm n 2 map

fillPrimesMap :: M.IntMap Int
fillPrimesMap = let fpm c mp = if c > 20 then mp
                               else fpm (c + 1) (updatePrimesMap c mp)
                in fpm 2 M.empty

p5 :: Int
p5 = M.foldlWithKey f 1 fillPrimesMap
 where f result k a = result * k ^ a
