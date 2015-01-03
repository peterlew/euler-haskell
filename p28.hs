
sumInNxN :: Int -> Int
sumInNxN 1 = 1 
sumInNxN n = sumInNxN (n - 2) + 4 * (n - 2)^2 + 10 * (n - 1) 


