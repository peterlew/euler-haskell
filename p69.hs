
import Useful
import qualified Data.Numbers.Primes as P
import qualified Data.Map as M

upperN :: Int
upperN = 10^6

initTotientMap :: M.Map Int Double
initTotientMap = M.fromList $ zip [1..upperN] (map fromIntegral [1..upperN])

sieveP :: Int -> M.Map Int Double -> M.Map Int Double
sieveP p mp = foldl f mp (map (* p) [1..(div upperN p)])
 where f m x = M.insertWith (*) x (1.0 - 1.0 / fromIntegral p) m

allTotients :: M.Map Int Double
allTotients = foldr sieveP initTotientMap (takeWhile (< upperN) P.primes)

totientRatios :: M.Map Int Double
totientRatios = M.mapWithKey f allTotients
 where f k t = fromIntegral k / t

getMaxFromMap :: Ord b => M.Map a b -> [a]
getMaxFromMap m = f [] Nothing (M.toList m)
 where f ks _ [] = ks
       f ks Nothing ((k,v):rest) = f (k:ks) (Just v) rest
       f ks (Just u) ((k,v):rest)
           | v < u = f ks (Just u) rest
           | v > u = f [k] (Just v) rest
           | otherwise = f (k:ks) (Just v) rest
