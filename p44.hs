
uB :: Int
uB = 10000

posQuadRoot :: Int -> Int -> Int -> Maybe Int
posQuadRoot a b c = 
    let x = div ((-1) * b + floor (sqrt (fromIntegral (b^2 - 4 * a * c)))) (2 * a)
    in if a * x^2 + b * x + c == 0 then Just x
       else Nothing

nKWorks :: Int -> Int -> Bool
nKWorks n k = 
    let c = (n + k) - 3 * (n^2 + k^2)
    in case posQuadRoot 3 (-1) c of
        Nothing -> False
        Just x -> let c2 = (n - k) - 3 * (n^2 - k^2)
                  in case posQuadRoot 3 (-1) c2 of 
                      Nothing -> False
                      Just z -> True

p44 :: (Int, Int)
p44 = [(n, k) | n <- [1..uB], k <- [1..uB], nKWorks n k]!!1

nthPentagonal :: Integer -> Integer
nthPentagonal n = div (n * (3 * n - 1)) 2
