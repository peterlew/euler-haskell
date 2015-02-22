
import Useful (isSquare)

test :: Integer -> Bool
test n = isSquare (5 * n^2 + 14 * n + 1)

nuggets :: [Integer]
nuggets = filter test [1..]

--wow, this recurrence was really nasty
--I need to learn how to do these mathematically
--but I guess I'm pretty good at the patterns

nuggets1 :: [Integer]
nuggets1 = 
    let f n | n < 1 = 0
        f 1 = 2
        f 2 = 5
        f n = 7 * f (n - 2) - f (n - 4) + 7
    in map f [1..]

--haskellize it 

nuggets2 :: [Integer]
nuggets2 = 2 : 5 : zipWith f nuggets2 (0 : 0 : nuggets2)
 where f n1 n2 = 7 * n1 - n2 + 7