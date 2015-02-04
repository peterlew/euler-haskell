
rMax :: Integer -> Integer
rMax a = let aSqr = a^2
             f n = mod ((a - 1)^n + (a + 1)^n) aSqr
         in maximum $ map f [1..(2*a)]

