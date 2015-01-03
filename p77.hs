
import Data.Maybe
import Data.List
import qualified Data.Numbers.Primes as P

type Partition = [Int]

primePartitionsToUnder :: Int -> Int -> [Maybe Partition]
primePartitionsToUnder 0 _ = [Just []] 
primePartitionsToUnder _ 0 = [Nothing]
primePartitionsToUnder n cap | n < cap = primePartitionsToUnder n n
primePartitionsToUnder n cap = foldl (++) [] (map f (takeWhile (<= cap) P.primes))
 where f x = map (g x) (primePartitionsToUnder (n - x) x)
       g _ Nothing = Nothing
       g x (Just p) = Just (x:p)

primePartitions :: Int -> [Partition]
primePartitions n = map (maybe [] id) (filter isJust $ primePartitionsToUnder n (n - 1))

p77 :: Maybe Int
p77 = find f [1..]
 where f n = length (primePartitions n) > 5000