
p48 :: String
p48 = reverse $ take 10 $ reverse $ show $ foldr (+) 0 (map f [1..1000])
 where f n = n^n