
-- bounds: 10^(n-1) <= m^n < 10^n gives
-- a) m < 10
-- b) n <= 1 / (1 - log m / log 10)

upperM :: Integral a => a
upperM = 9

upperN :: Integral a => a
upperN = 21

nthPowers :: Int -> [Integer]
nthPowers n = map ((flip (^)) n) [1..upperM]

nDigitNPowers :: Int -> [Integer]
nDigitNPowers n = dropWhile (< 10^(n-1)) (takeWhile (< 10^n) (nthPowers n))

allNDigitNPowers :: [Integer]
allNDigitNPowers = foldr (++) [] (map nDigitNPowers [1..upperN])


