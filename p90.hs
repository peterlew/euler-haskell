
import Data.List

type Cube = [Int]

allCubes :: [Cube]
allCubes =
    let ac _ _ 0 = [[]]
        ac cur 0 lft = [[cur..(cur + lft - 1)]]
        ac cur skp lft = map (cur:) (ac (cur + 1) skp (lft - 1)) ++ ac (cur + 1) (skp - 1) lft
    in ac 0 4 6

squarePairs :: [(Int, Int)]
squarePairs = [(0, 1), (0, 4), (0, 9), (1, 6), (2, 5),
               (3, 6), (4, 9), (6, 4), (8, 1)]

areValidCubes :: Cube -> Cube -> Bool
areValidCubes cb1 cb2 = all f squarePairs
 where f (a, b) = myElem a cb1 && myElem b cb2 ||
                  myElem a cb2 && myElem b cb1
       myElem 6 xs = elem 6 xs || elem 9 xs
       myElem 9 xs = elem 6 xs || elem 9 xs
       myElem x xs = elem x xs

validCubes :: [[Cube]]
validCubes = nub $ map sort candCubes
 where candCubes = [ [c1, c2] | c1 <- allCubes, c2 <- allCubes, c1 <= c2, areValidCubes c1 c2 ]