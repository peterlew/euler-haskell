numLetterCounts :: Int
numLetterCounts = (sum $ map length (numWords)) - 27
 where numWords = let otn = ["", "one", "two", "three", "four", "five",
                             "six", "seven", "eight", "nine"]
                      multsOfTen = ["twenty", "thirty", "forty", "fifty",
                                    "sixty", "seventy", "eighty", "ninety"]
                      oth = otn 
                         ++ ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen",
                             "sixteen", "seventeen", "eighteen", "nineteen"] 
                         ++ foldl (++) [] (map (\w -> zipWith (++) (replicate 10 w) otn) multsOfTen)
                      ott = oth
                         ++ foldl (++) [] (map (\w -> zipWith (++) (replicate 100 w) oth) (map (\w -> w ++ "hundredand") (tail otn)) )
                         ++ ["onethousand"]
                  in ott


















































