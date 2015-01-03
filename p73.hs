
upperN :: Int
upperN = 12000

countNForD :: Int -> Int
countNForD d = length $ filter f [(div d 3 + 1)..(div d 2)]
 where f n = gcd d n == 1

p73 :: Int
p73 = foldl (+) 0 (map countNForD [2..upperN]) - 1
