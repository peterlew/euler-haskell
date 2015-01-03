
import Useful
import Data.List

possibleTwos :: Float -> Float -> [Float]
possibleTwos a 0 = [ a, 0, negate a]
possibleTwos 0 b = [ b, 0, negate b]
possibleTwos a b = [ a + b, a - b, b - a, a * b, a / b, b / a ]

possibleThrees :: Float -> Float -> Float -> [Float]
possibleThrees a b c = (concat $ map (possibleTwos a) (possibleTwos b c)) ++
                       (concat $ map (possibleTwos b) (possibleTwos a c)) ++
                       (concat $ map (possibleTwos c) (possibleTwos a b))

possibleFours :: Float -> Float -> Float -> Float -> [Float]
possibleFours a b c d = (concat $ map (possibleTwos a) (possibleThrees b c d)) ++
                        (concat $ map (possibleTwos b) (possibleThrees a c d)) ++ 
                        (concat $ map (possibleTwos c) (possibleThrees a b d)) ++
                        (concat $ map (possibleTwos d) (possibleThrees a b c)) ++
                        (concat [ possibleTwos x y | x <- possibleTwos a b, y <- possibleTwos c d ]) ++
                        (concat [ possibleTwos x y | x <- possibleTwos a c, y <- possibleTwos b d ])

hitNums :: Float -> Float -> Float -> Float -> [Int]
hitNums a b c d = sort $ nub $ map (round) $ filter isInt $ filter (> 0) $ possibleFours a b c d
 where isInt x = x == fromIntegral (round x)

streakCount :: Float -> Float -> Float -> Float -> Int
streakCount a b c d = head $ [1..] \\ hitNums a b c d

possibleSets :: [[Float]]
possibleSets =
    let ps cur skp lft = 
         if lft == 0 then [[]]
         else if skp == 0 then [[cur..(cur + lft - 1)]]
              else map (cur :) (ps (cur + 1) skp (lft - 1)) ++ ps (cur + 1) (skp - 1) lft
    in ps 0 6 4

main' :: [Float]
main' = maxingVal f possibleSets
 where f [a, b, c, d] = streakCount a b c d