
--six digits or fewer since 7 * 9^5 = a six-digit number

p30 :: Int
p30 = let p30' n tot = if n >= 1000000 then tot
                       else if isFifthPowerSum n then p30' (n + 1) (tot + n)
                            else p30' (n + 1) tot
      in p30' 2 0
 where isFifthPowerSum n = foldl (\a b -> a + b^5) 0 (map (\c -> fromEnum c - fromEnum '0') $ show n) == n