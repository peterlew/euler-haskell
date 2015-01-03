
import Data.List
import qualified Data.Numbers.Primes as P

topSum :: Int
topSum = 50000000

sqrs, cubes, frths :: [Int]
sqrs  = takeWhile (< topSum) (map (^2) P.primes)
cubes = takeWhile (< topSum) (map (^3) P.primes)
frths = takeWhile (< topSum) (map (^4) P.primes)

addNs :: [Int] -> Int -> [Int]
addNs ns prev = let upperN = topSum - prev
                 in nub $ map (+ prev) (takeWhile (< upperN) ns)

addNsToAll :: [Int] -> [Int] -> [Int]
addNsToAll ns xs = nub $ foldl (++) [] (map (addNs ns) xs)

main' :: Int
main' = length $ xs
 where xs = addNsToAll sqrs (addNsToAll cubes frths)
