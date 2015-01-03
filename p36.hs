
isPalindrome :: Int -> Bool
isPalindrome n = 
    let s = show n
    in s == reverse s

toBinaryString :: Int -> String 
toBinaryString 1 = "1"
toBinaryString n = if mod n 2 == 0 then toBinaryString (div n 2) ++ "0"
                   else toBinaryString (div n 2) ++ "1"

isBinaryPalindrome :: Int -> Bool
isBinaryPalindrome n =
    let s = toBinaryString n
    in s == reverse s

p36 :: Int
p36 = sum $ filter (\n -> isPalindrome n && isBinaryPalindrome n) [1..999999]                        