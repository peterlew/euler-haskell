
import Useful

champernowne :: String
champernowne = foldr (++) "" (map show [0..])

p40 :: Int
p40 = foldr (*) 1 (map (\n -> cToD $ champernowne!!(10^n)) [0..6])