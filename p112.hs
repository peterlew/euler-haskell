
isBouncy :: Int -> Bool
isBouncy x = 
    let f inc dec [c] = inc * dec == 0
        f inc dec (c1:c2:cs) = inc * dec == 0 && 
                               f (if c1 < c2 then inc + 1 else inc)
                                 (if c1 > c2 then dec + 1 else dec)
                                 (c2:cs)
    in not $ f 0 0 (show x)

p112 = 
 let f c tot = 
      let newTot = tot + if isBouncy c then 1 else 0
      in if fromIntegral newTot / fromIntegral c == 0.99 then c
         else f (c + 1) newTot
 in f 1 0