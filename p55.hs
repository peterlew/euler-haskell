
isPalindrome :: Integer -> Bool
isPalindrome n = 
    let s = show n
    in s == reverse s

step :: Integer -> Integer
step n = n + read (reverse (show n))

stepChain :: Integer -> [Integer]
stepChain n = step n : (stepChain $ step n)

isLychrel :: Integer -> Bool
isLychrel n = all (not . isPalindrome) (take 50 $ stepChain n)

p55 :: Int
p55 = length $ filter isLychrel [1..9999]