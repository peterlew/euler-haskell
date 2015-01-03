
import Useful
import Data.Maybe
import Control.Monad.State
import qualified Data.IntMap as M

twoMinusRtTwo :: Double
twoMinusRtTwo = 2.0 - sqrt 2.0

expectedR :: Integer
expectedR = 292893218813

expectedSqr :: Integer
expectedSqr = 8 * (expectedR^2 + 1) + 1

expectedRt :: Integer
expectedRt = squareRoot expectedSqr

validSqr :: Integer -> Bool
validSqr n | mod n 2 == 0 = False
validSqr n | mod (n - 1) 8 > 0 = False
validSqr n = isSquare (div (n - 1) 8)

--okay, this is crazy ... if you're looking at this, compare 
-- filter validSqr (map (^2) [1..])     with      OEIS A055997

-- why? is every other square triangular number valid?
-- or one could say why are the odd square triangular numbers valid?
-- All odd triangular numbers, if square, equal 8*r^2 + 1 
-- it gets really good, Pell's equation is involved 
-- http://en.wikipedia.org/wiki/Square_triangular_number

mInit :: M.IntMap Integer
mInit = M.fromList [(0, 1), (1, 2)]

nthValidSqr :: Int -> State (M.IntMap Integer) Integer
nthValidSqr n = do
    m <- get
    if M.member n m then return (m M.! n)
    else do
        anm1 <- nthValidSqr (n - 1)
        anm2 <- nthValidSqr (n - 2)
        let res = 6 * anm1 - anm2 - 2
        modify (M.insert n res)
        return res

ourSqr :: Integer
ourSqr = evalState (nthValidSqr 32) mInit

r :: Integer
r = squareRoot (div (ourSqr - 1) 8)

b :: Integer
b = div (2 * r + squareRoot ourSqr + 1) 2

