import Data.List
import Useful

validProducts :: String -> [Int]
validProducts s = foldl (++) [] $ map f [1..4] 
 where f n = let multiplicand = read $ take n s 
                 readIn x = mapTuple read $ splitAt x (drop n s) 
                 multpliersAndProds = map readIn [1..(9 - 2 * n)] 
             in map (\(m, p) -> p) $ filter (\(m, p) -> multiplicand * m == p ) multpliersAndProds

allProducts :: [Int]
allProducts = nub $ foldl (++) [] $ map validProducts $ permutations "123456789"
