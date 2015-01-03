
import Data.List

--n is at most 6. x = 1, n = 9 doesn't work and x = 2, n = 7 gives 10 digits
--x is at most 4 digits

isPandigital :: String -> Bool
isPandigital s | length s > 9 = False 
isPandigital s = sort s == "123456789"

concatXWithNProd :: Int -> Int -> String
concatXWithNProd x n = foldl (++) "" (map (\t -> show (x * t)) [1..n])

allPandigitals :: [String]
allPandigitals = filter isPandigital [ concatXWithNProd x n | x <- [1..9999], n <- [1..6]]




