
import Data.List
import qualified Data.Map as M

maxN :: Integral a => a
maxN = 10000

cubes :: [Integer]
cubes = map ((flip (^)) 3) [1..maxN]

sortedDigitCubes :: [String]
sortedDigitCubes = map (sort . show) cubes

hasNPermutations :: Int -> Integer -> Bool
hasNPermutations n x = length (filter (== (sort $ show x)) sortedDigitCubes) == n

p62 :: Maybe Integer
p62 = find (hasNPermutations 5) cubes