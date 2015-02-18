
maxP :: Integer
maxP = 10^8

maxM :: Integer
maxM = floor ((sqrt (2 * fromIntegral maxP - 1) - 1) / 2)

maxN :: Integer -> Integer
maxN m = div (maxP - 2 * m^2 - 1) (2 * m)

maxK :: Integer -> Integer -> Integer
maxK m n = div (maxP - 1) (2 * m^2 + 2 * m * n)

test :: Integer -> Integer -> Bool
test m n = mod (m^2 + n^2) (m^2 - n^2 - 2 * m * n) == 0

allTrips :: [(Integer, Integer, Integer)]
allTrips = [ 
             (k, m, n) | 
                m <- [1..maxM], 
                n <- [1..min m (maxN m)],
                k <- [1..maxK m n],
                gcd m n == 1,
                mod (m - n) 2 == 1,
                test m n
           ]

main = do
    print (length allTrips)