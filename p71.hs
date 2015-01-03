
import Useful
import Data.List

reduce :: (Int, Int) -> (Int, Int)
reduce (a, b) = (div a d, div b d)
 where d = gcd a b

p71 :: (Int, Int)
p71 = 
    let p71' bst bVal cur | cur > 1000000 = reduce (aFromB bVal, bVal)
        p71' bst bVal cur = let a = aFromB cur
                                diff = dFromAB a cur
                            in if diff > 0 && diff < bst 
                               then p71' diff cur (cur + 1)
                               else p71' bst bVal (cur + 1)
    in p71' 3 6 1
 where aFromB b = div (3 * b) 7
       dFromAB a b = fromIntegral (3 * b - 7 * a) / fromIntegral (7 * b) 
