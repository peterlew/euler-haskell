
import Useful (squareRoot, isSquare)

--if the progression is r, q, d then n = r*(x^3*r + 1)
--it could also be q, r, d, (n = qx(qx + 1)) but this 
--never leads to a square (not proven)

--for n = r(x^3r + 1), let x = a / b, a > b, a and b coprime
--r must equal k * b^2 for n to be an integer
--it can be shown that there are some pretty good bounds on b, a, and k

maxN :: Integer
maxN = 10^12

possBs :: [Integer]
possBs = takeWhile f [1..]
 where f b = b^4 + b^2 < maxN

possAs :: Integer -> [Integer]
possAs b = filter (\a -> gcd a b == 1) $ takeWhile f [(b + 1)..]
 where f a = a^3 <= div maxN b - b

possKs :: Integer -> Integer -> [Integer]
possKs a b = takeWhile f [1..]
 where f k = k <= div (-b^2 + squareRoot (b^4 + 4 * a^3 * b * maxN)) (2 * a^3 * b)

possNs :: [Integer]
possNs = filter (< maxN) poss 
 where poss = [ k^2 * a^3 * b + k * b^2 |
                b <- possBs,
                a <- possAs b,
                k <- possKs a b ]

squareNs :: [Integer]
squareNs = filter isSquare possNs