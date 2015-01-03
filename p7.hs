
p7 :: Int
p7 = let p7' n ps count = if count == 10001 then last ps
                          else if isDivisibleByAny n ps then p7' (n + 1) ps count
                               else p7' (n + 1) (ps ++ [n]) (count + 1)
     in p7' 2 [] 0
    where isDivisibleByAny n [] = False
          isDivisibleByAny n (p:ps) = mod n p == 0 || isDivisibleByAny n ps

