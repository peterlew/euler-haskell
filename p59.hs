
import Data.List
import Data.List.Split
import Data.Bits
import Useful

xOrWith :: [Int] -> [Int] -> [Int]
xOrWith ks = zipWith xor ks

decrypt :: [Int] -> [Int] -> [Int]
decrypt ks = xOrWith (cycle ks)

msgToText :: [Int] -> String
msgToText xs = foldr (++) "" (map f xs)
 where f x = [toEnum x]

allDecryptions :: [Int] -> [String]
allDecryptions ns = 
    let ltrRange = [97..122]
    in [msgToText $ decrypt [a, b, c] ns | a <- ltrRange, b <- ltrRange, c <- ltrRange]

main = do
    handle <- readFile "assets/p59.txt"
    let nums = map read (splitOn "," ((lines handle)!!0))
        origText = maybe "" id $ find (isInfixOf "exists") $ allDecryptions nums
    return $ sum $ map fromEnum origText
