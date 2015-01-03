
main = do
    ls <- readFile "assets/p13.txt"
    let numStrings = lines ls
    let nums :: [Integer]
        nums = map read numStrings
    return $ take 10 (show $ sum nums)