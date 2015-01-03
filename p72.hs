
import qualified Data.Numbers.Primes as P
import qualified Data.Map as M

upperN :: Int
upperN = 10^6

candidatePrimes :: [Int]
candidatePrimes = takeWhile (<= upperN) P.primes

initTotients :: M.Map Int Float
initTotients = M.fromList $ zip [2..upperN] (map fromIntegral [2..upperN])

sieveP :: Int -> M.Map Int Float -> M.Map Int Float
sieveP p mp = foldl f mp (map (* p) [1..(div upperN p)])
 where f m k = M.insertWith (*) k (1.0 - 1.0 / fromIntegral p) m

allTotients :: M.Map Int Integer
allTotients = M.map round (foldr sieveP initTotients candidatePrimes)

p72 :: Integer
p72 = M.fold (+) 0 allTotients