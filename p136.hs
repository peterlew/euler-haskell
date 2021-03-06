
import Useful (count)
import qualified Data.Numbers.Primes as P

--unique solution means n is a prime with p = 3 mod 4
--or n is a number k*p where k is 4 or 16, p prime
--it is easy to prove why these cases provide a unique solution, 
--but I haven't proved why they are the ONLY cases

cap :: Integer
cap = 50000000

firstCase :: Int
firstCase = count f (takeWhile (< cap) P.primes)
 where f n = mod n 4 == 3

twos :: [Integer]
twos = [4, 16]

secondCase :: Int
secondCase = (length $ takeWhile (< cap) twos) +
             (sum $ map f (tail (takeWhile (<= div cap 4) P.primes)))
 where f p = length $ takeWhile (< cap) (map (*p) twos)

numWithOneSol :: Int
numWithOneSol = firstCase + secondCase

main = do
    print numWithOneSol