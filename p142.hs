
import Data.List
import Useful (isSquare)

type Trip = (Integer, Integer)

maxM :: Integer
maxM = 20

maxK :: Integer
maxK = 20

genDoubles :: [Trip]
genDoubles = [ (k * m^2 - k * n^2, 2 * k * m * n) | 
               m <- [1..maxM],
               n <- [1..(m - 1)],
               k <- [1..maxK],
               gcd m n == 1 ]

uniqueDoubles :: [Trip]
uniqueDoubles =
    let f _ [] = []
        f (a1, b1) ((a2, b2):ts) = 
            if (a1 == a2 && b1 == b2) || (a1 == b2 && b1 == a2) then f (a1, b1) ts
            else (a2, b2) : f (a1, b1) ts
        remHeadDups [] = []
        remHeadDups (l:ls) = l : remHeadDups (f l ls)
    in remHeadDups genDoubles

sortedDoubles :: [Trip]
sortedDoubles = sort $ map tupSort uniqueDoubles
 where tupSort (a, b) = if a > b then (b, a) else (a, b)

candsFor :: Trip -> [Trip] -> [(Trip, Trip)]
candsFor _ [] = []
candsFor (a1, b1) ((a2, b2):ts) = 
    if length (nub [a1, b1, a2, b2]) == 3 then ((a1, b1), (a2, b2)) : candsFor (a1, b1) ts
    else candsFor (a1, b1) ts    

allCands :: [(Trip, Trip)]
allCands = 
    let f [] = []
        f (t:ts) = candsFor t ts ++ f ts
    in f sortedDoubles

checkTripPair :: (Trip, Trip) -> Bool
checkTripPair ((a1, b1), (a2, b2)) = 
    let ns = [a1, b1, a2, b2]
        b = dblElem ns
        bInds = elemIndices b ns
        a = (ns!!(1 - bInds!!0))^2
        c = a2^2 + b2^2
    in isSquare (a + c)
 where dblElem [] = 0
       dblElem (x:xs) = if elem x xs then x
                        else dblElem xs

main = do 
    print $ filter checkTripPair allCands