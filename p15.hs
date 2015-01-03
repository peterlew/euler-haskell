
fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n - 1)

routes :: Integer -> Integer
routes n = div (foldl (*) 1 [(n + 1) .. (2 * n)]) (fac n)


