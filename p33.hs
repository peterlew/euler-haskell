
isWeirdFrac :: (Int, Int) -> (Int, Int) -> Bool    
isWeirdFrac (n1, n2) (d1, d2) | any (== 0) [n1, n2, d1, d2] = False   
isWeirdFrac (n1, n2) (d1, d2) | n1 == d1 && n2 == d2 = False    
isWeirdFrac (n1, n2) (d1, d2) | 10 * n1 + n2 >= 10 * d1 + d2 = False                    
isWeirdFrac (n1, n2) (d1, d2) = let realVal = fromIntegral (10 * n1 + n2) / fromIntegral (10 * d1 + d2)
                                in f realVal n1 d1 n2 d2 || 
                                   f realVal n1 d2 n2 d1 ||
                                   f realVal n2 d1 n1 d2 ||
                                   f realVal n2 d2 n1 d1 
 where f v n1 d1 n2 d2 = n1 == d1 && v == fromIntegral n2 / fromIntegral d2

findWeirdFracs :: [((Int, Int), (Int, Int))]
findWeirdFracs = 
    let fwf n1 n2 d1 d2 fracs = if d2 > 9 then 
                                 if d1 > 8 then
                                  if n2 > 8 then
                                   if n1 > 8 then fracs
                                   else fwf (n1 + 1) 1 1 1 fracs
                                  else fwf n1 (n2 + 1) 1 1 fracs
                                 else fwf n1 n2 (d1 + 1) 1 fracs
                                else if isWeirdFrac (n1, n2) (d1, d2) 
                                      then fwf n1 n2 d1 (d2 + 1) (((n1, n2), (d1, d2)):fracs)
                                     else fwf n1 n2 d1 (d2 + 1) fracs
    in fwf 1 1 1 1 []

prod :: (Int, Int)
prod = foldl f (1, 1) $ map toFracTuple findWeirdFracs
 where toFracTuple ((n1, n2), (d1, d2)) = (10 * n1 + n2, 10 * d1 + d2)
       f (n1, d1) (n2, d2) = (n1 * n2, d1 * d2)


