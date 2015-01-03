
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = 
    let largestPrimeFactor' n c = if c > div n 2 then n
                                  else if mod n c == 0 then largestPrimeFactor' (div n c) c
                                  else largestPrimeFactor' n (c + 1)
    in largestPrimeFactor' n 2
