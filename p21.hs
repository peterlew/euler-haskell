
sumOfDivisors :: Int -> Int
sumOfDivisors n = let stop = floor $ sqrt $ fromIntegral n
                      sod n c tot = if c > stop then tot
                                    else if mod n c == 0 then 
                                          if c * c == n then sod n (c + 1) (tot + c)
                                          else sod n (c + 1) (tot + c + div n c)
                                         else sod n (c + 1) tot
                  in sod n 2 0 + 1

p21 :: Int
p21 = let p21' n tot = if n >= 10000 then tot
                       else if isAmicable n then p21' (n + 1) (tot + n)
                            else p21' (n + 1) tot
      in p21' 1 0
    where isAmicable n1 = let n2 = sumOfDivisors n1
                          in n1 == sumOfDivisors n2 && n1 /= n2