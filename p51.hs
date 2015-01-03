
import Useful
import Data.Maybe
import Data.List
import qualified Data.Numbers.Primes as P

numGeneratedPrimes :: [Maybe String] -> Int
numGeneratedPrimes cs | all isJust cs = 0
numGeneratedPrimes (Just "0":_) = 0
numGeneratedPrimes cs = let digs = if cs!!0 == Nothing then [1..9] else [0..9]
                        in length $ filter f digs
 where f n = P.isPrime $ read $ foldr (++) "" (map (maybe (show n) id) cs)

sToTemplates :: String -> [[Maybe String]]
sToTemplates s | length s == 1 = [[Just s], [Nothing]]
sToTemplates s =
    let rst = sToTemplates $ tail s 
    in map f rst ++ map g rst 
 where f t = Just [head s] : t
       g t = Nothing : t

bestFamilyForX :: Int -> [Maybe String]
bestFamilyForX x = maxingVal numGeneratedPrimes (sToTemplates $ show x)

bestFamilyValForX :: Int -> Int
bestFamilyValForX x = maximum $ map numGeneratedPrimes(sToTemplates $ show x)

p51 :: Maybe Int
p51 = find f P.primes
 where f p = bestFamilyValForX p == 8