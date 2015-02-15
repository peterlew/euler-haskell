import Data.List

aOfN :: Integer -> Int
aOfN n | mod n 2 == 0 = 0
aOfN n | mod n 5 == 0 = 0
aOfN n =
    let f cur tenPw = if cur == 0 then 1
                      else 1 + f (mod (cur + tenPw) n) (mod (10 * tenPw) n)
    in f 1 10 
