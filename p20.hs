
fac :: Integer -> Integer
fac 1 = 1
fac n = n * fac (n - 1)