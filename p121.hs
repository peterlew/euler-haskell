
type Frac = (Integer, Integer)

probOfString :: String -> Frac
probOfString s =
    let f [] _ = (1, 1)
        f ('R':s) c = fMult (c - 1, c) (f s (c + 1))
        f ('B':s) c = fMult (1, c) (f s (c + 1))
    in f s 2 

fMult :: Frac -> Frac -> Frac
fMult (n1, d1) (n2, d2) = (n1 * n2, d1 * d2)

fAdd :: Frac -> Frac -> Frac
fAdd (n1, d1) (n2, d2) = fSimp $ (n1 * d2 + n2 * d1, d1 * d2)

fSimp :: Frac -> Frac
fSimp (n, d) = let g = gcd n d in (div n g, div d g)

rbStrings :: Int -> Int -> [String]
rbStrings r 0 = [replicate r 'R']
rbStrings 0 b = [replicate b 'B']
rbStrings r b = map ('R':) (rbStrings (r - 1) b) ++
                map ('B':) (rbStrings r (b - 1))

winningStrings :: Int -> [String]
winningStrings turns =
    concat $ map f [(div turns 2 + 1)..turns]
 where f b = rbStrings (turns - b) b

probOfWinning :: Int -> Frac
probOfWinning turns = foldl fAdd (0, 1) $ map probOfString (winningStrings turns)

maxPrize :: Int -> Integer
maxPrize turns =
    let (n, d) = probOfWinning turns
    in div d n - 1

