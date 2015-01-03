
tryGivenA :: Int -> Maybe (Int, Int, Int)
tryGivenA a = let tga b = if b + a >= 1000 then Nothing
                          else let c = 1000 - b - a
                               in if a^2 + b^2 == c^2 then Just (a, b, c)
                                  else tga (b + 1)
              in tga 1

tryAll :: Maybe (Int, Int, Int)
tryAll = let ta a = if a >= 999 then Nothing
                    else case tryGivenA a of
                          Nothing -> ta (a + 1)
                          Just triple -> Just triple
         in ta 1