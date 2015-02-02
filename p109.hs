
singleVals :: [String]
singleVals = map ('S':) (map show ([1..20] ++ [25]))

doubleVals :: [String]
doubleVals = map ('D':) (map show ([1..20] ++ [25]))

tripleVals :: [String]
tripleVals = map ('T':) (map show [1..20])

allVals :: [String]
allVals = "M" : (singleVals ++ doubleVals ++ tripleVals)

vToScore :: String -> Int
vToScore "M" = 0
vToScore ('S':num) = read num
vToScore ('D':num) = 2 * read num
vToScore ('T':num) = 3 * read num

waysToCheckoutFrom :: Int -> Int
waysToCheckoutFrom n = 
    sum $ map (\x -> waysToTwoTo (n - x)) (takeWhile (<= n) (map vToScore doubleVals))

waysToTwoTo :: Int -> Int
waysToTwoTo n | n < 2 = 1
waysToTwoTo n | n > 120 = 0
waysToTwoTo n = length [(a, b) | a <- allVals, b <- allVals, 
                                 a < b, vToScore a + vToScore b == n]
                + (if mod n 2 == 0 && (n <= 40 || n == 50) then 1 else 0)
                + (if mod n 4 == 0 && (n <= 80 || n == 100) then 1 else 0)
                + (if mod n 6 == 0 then 1 else 0)

waysToCheckoutUnder :: Int -> Int
waysToCheckoutUnder n =
    sum $ map waysToCheckoutFrom [1..(n - 1)]

