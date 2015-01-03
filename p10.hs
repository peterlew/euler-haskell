
import qualified Data.IntMap as M

upperBnd = 2000000

primes :: M.IntMap Bool
primes = let prms c mp = if c >= upperBnd then mp
                         else case M.lookup c mp of
                               Nothing -> prms (c + 1) (markMultiples (c + c) c (M.insert c True mp))
                               Just _  -> prms (c + 1) mp
         in prms 2 (M.fromList [(1, False)])
    where markMultiples c orig mp = if c >= upperBnd then mp
                                    else markMultiples (c + orig) orig (M.insert c False mp)

p10 :: Integer
p10 = M.foldlWithKey f 0 primes
 where f res k a = if a then res + fromIntegral k else res