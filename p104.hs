
import qualified Data.IntMap as M
import Control.Monad.State
import Useful (leave)
import Data.List

fib :: Int -> State (M.IntMap Integer) Integer
fib n = do
    m <- get
    if M.member n m then return $ m M.! n
    else if n < 3 then do
          modify (M.insert n 1)
          return 1
         else do 
          f1 <- fib (n - 1)
          f2 <- fib (n - 2)
          let res = f1 + f2
          modify (M.insert n res)
          return res

pandigitalEnds :: Integer -> Bool
pandigitalEnds x = 
    let s = show x
    in pand (take 9 s) && pand (leave 9 s)
 where pand str = sort str == "123456789"

pandigitalFront :: Integer -> Bool
pandigitalFront x = sort (take 9 (show x)) == "123456789"

p104 :: State (M.IntMap Integer) Int
p104 = 
 let p104' c = do
      res <- fib c
      if pandigitalEnds res then return c else p104' (c + 1)
 in p104' 1 

p104pt2 :: Integer
p104pt2 =
 let f f1 f2 ct = 
      let nxt = f1 + f2
      in if pandigitalEnds nxt then ct
         else f f2 nxt (ct + 1)
 in f 1 1 3

p104pt3 :: Integer
p104pt3 = 
    let f f1 f2 e1 e2 ct =
         let nxt = f1 + f2
             nxtE = mod (e1 + e2) 1000000000
         in if pandigital (take 9 (show nxt)) && pandigital (show nxtE) then ct
            else f f2 nxt e2 nxtE (ct + 1)
    in f 1 1 1 1 3
 where pandigital s = sort s == "123456789"