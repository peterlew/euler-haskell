
isPalindrome :: Int -> Bool
isPalindrome n = let s = show n
                 in s == reverse s

p4 :: Int
p4 = let p4' n1 n2 best = let prod = n1 * n2
                              nxtBest = max best ((fromEnum $ isPalindrome prod) * prod)
                          in if n2 > 999 then 
                              if n1 > 999 then nxtBest
                              else p4' (n1 + 1) 100 nxtBest
                             else p4' n1 (n2 + 1) nxtBest  
     in p4' 100 100 0