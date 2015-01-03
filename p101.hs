
diffs :: [Integer] -> [Integer]
diffs ns | length ns == 1 = []
diffs (n1:(n2:ns)) = (n2 - n1) : diffs (n2:ns)

guessNext :: [Integer] -> Integer
guessNext ns = 
    let ds = diffs ns
    in if all (== head ds) ds then last ns + head ds
       else last ns + guessNext ds

uOfN :: Integer -> Integer
uOfN n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
--uOfN n = n^3

nthFIT :: Integer -> Integer
nthFIT 1 = uOfN 1
nthFIT n = guessNext (map uOfN [1..n])