
import Useful

isReversible :: Integer -> Bool
isReversible n | mod n 10 == 0 = False 
isReversible n = 
    all (\x -> mod x 2 == 1) $ map cToD (show (n + (read (reverse (show n)))))

main = do
    print $ count isReversible [1..10^9 - 1]

construct2 :: [[Int]]
construct2 =
    [ [a, b] | b <- [1..9], a <- [1..9-b], mod (a + b) 2 == 1]

construct3 :: [[Int]]
construct3 = 
 [ [a, b, c] | b <- [0..9], c <- [1..9], a <- [10-c..9], mod (a + c) 2 == 1,
   not (a + c > 9 && b > 4)]

construct4 :: [[Int]]
construct4 = 
 [ [a, b, c, d] | a <- [1..9], b <- [0..9], c <- [0..9-b], d <- [1..9-a],
   mod (a + d) 2 == 1, mod (b + c) 2 == 1]

--there's no 5s! Not hard to prove
--btw my brute force finished when I got here and gives 608720
--so I got that going for me, which is nice
construct5 :: [[Int]]
construct5 =
 [ [a, b, c, d, e] | c <- [0..4], b <- [0..9], let d = 9 - b, a <- [1..9],
   e <- [10-a..9], mod (a + e) 2 == 1, mod (b + d) 2 == 0 ]

