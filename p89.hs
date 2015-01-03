
import Useful

cNum :: Char -> Int
cNum c = case c of
          'I' -> 1
          'V' -> 5
          'X' -> 10
          'L' -> 50
          'C' -> 100
          'D' -> 500
          'M' -> 1000

stringToNum :: String -> Int
stringToNum [] = 0
stringToNum [c] = cNum c
stringToNum (c:n:cs) =
    if (c == 'I' && elem n ['V', 'X']) || 
       (c == 'X' && elem n ['L', 'C']) ||
       (c == 'C' && elem n ['D', 'M']) then cNum n - cNum c + stringToNum cs
    else cNum c + stringToNum (n:cs)

numToString :: Int -> String
numToString n 
    | n < 5 = replicate n 'I'
    | n < 10 = 'V' : numToString (n - 5)
    | n < 50 = replicate (div n 10) 'X' ++ numToString (mod n 10)
    | n < 100 = 'L' : numToString (n - 50)
    | n < 500 = replicate (div n 100) 'C' ++ numToString (mod n 100)
    | n < 1000 = 'D' : numToString (n - 500)
    | otherwise = replicate (div n 1000) 'M' ++ numToString (mod n 1000)

reduceString :: String -> String
reduceString s = foldl f s reps
 where f str (from, to) = replace from to str
       reps = [("IIII", "IV"),
               ("VIV", "IX"),
               ("XXXX", "XL"),
               ("LXL", "XC"),
               ("CCCC", "CD"),
               ("DCD", "CM")]

main = do
    handle <- readFile "assets/p89.txt"
    let ls = lines handle
        diffs = map f ls
    return $ sum diffs
 where f s = let n = stringToNum s
             in length s - length (reduceString (numToString n))