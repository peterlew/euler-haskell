
import Control.Monad.State
import Data.List
import qualified Data.Numbers.Primes as P 
import qualified Data.IntMap as M 

maxC :: Int
maxC = 120000 - 1

addMultsOfP :: Int -> State (M.IntMap [Int]) ()
addMultsOfP p = do
    m <- get
    _ <- sequence $ map (\n -> modify (M.insertWith (++) (p * n) [p])) [1..(div maxC p)]
    return ()

addAllP :: State (M.IntMap [Int]) (M.IntMap [Int])
addAllP = do
    _ <- sequence $ map addMultsOfP (takeWhile (<= maxC) P.primes)
    get

distinctPrimeMap :: M.IntMap [Int]
distinctPrimeMap = evalState addAllP (M.fromList [(1,[1])])

isHit :: Int -> Int -> Int -> Bool
isHit a b c =
    let dPs = distinctPrimeMap M.! a ++
              distinctPrimeMap M.! b ++
              distinctPrimeMap M.! c
    in if length dPs == length (nub dPs) then
       let fis :: [Integer]
           fis = map fromIntegral dPs
       in product fis < fromIntegral c
    else False

triplesForC :: Int -> Int
triplesForC c = c * (length $ filter f [1..(div (c - 1) 2)])
 where f a = isHit a (c - a) c

main = 
    let f c tot = if c > maxC then print ("DONE: Total = " ++ show tot)
                  else do
                      let res :: Integer
                          res = fromIntegral $ triplesForC c
                      if mod c 1000 == 0 then do 
                               print ("Checked " ++ show c ++ 
                                      " with total " ++ show (tot + res))
                               f (c + 1) (tot + res)
                      else do
                           f (c + 1) (tot + res)
    in f 3 0