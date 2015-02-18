
import Useful (isSquare)

--it can be shown that AF(x) = x + x*AF(x) + x^2*AF(x)
--this gives AF(x) = -x / (x^2 + x - 1)
--and x = (-3 + sqrt( 5*n^2 + 2*n + 1)) / (2*n)

nuggets :: [Integer]
nuggets = filter f [1..]
 where f n = isSquare (5 * n ^2 + 2 * n + 1)

--by observation, the nuggets follow a recursion

nuggets2 :: [Integer]
nuggets2 = 2 : zipWith f nuggets2 (0 : nuggets2)
 where f n1 n2 = 7 * n1 - n2 + 1