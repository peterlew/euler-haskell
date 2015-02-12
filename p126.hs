
waysForLWH :: Int -> Int -> Int
waysForLWH x n = 
    let cap = div (x - 4 * n^2 + 4 * n - 2) (4 * n)
        w h = div (x - 4 * (n - 1) * (n - 2) - 4 * n * (h + 1) + 2 * h + 4) (2 * h + 4 * n - 2)
        l h w = let num = x - 4 * (n - 1) * (n - 2) - 2 * h * w - 4 * (n - 1) * (h + w)
                    den = 2 * h + 2 * w + 4 * n - 4
                in mod num den == 0 && div num den <= w
        cubes = [ l a b | a <- [1..cap], b <- [1..(min a (w a))] ]
    in length $ filter id cubes 

waysForX :: Int -> Int
waysForX x = sum $ map (waysForLWH x) (takeWhile (\n -> 4 * n^2 + 2 <= x) [1..])

main =
    let f (p:ps) = do
        let doub = 2 * p
            res = waysForX doub
        if res == 1000 then print ("FOUND IT! " ++ show doub ++ ": " ++ show res)
        else do
            print (show doub ++ ": " ++ show res)
            f ps
    in f [1..]

