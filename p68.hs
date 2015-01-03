
import Data.List
import Data.Maybe

type Trip = (Int, Int, Int)

makeRing :: [Trip] -> [Int] -> Int -> Int -> Int -> [Maybe [Trip]]
makeRing prev rem tot pre suf =
    if length rem == 1 then
        let el = rem!!0
        in if el + pre + suf == tot then [Just (prev ++ [(el, suf, pre)])]
           else [Nothing]
    else foldl (++) [] (map f rem)
 where f n = let rs = delete n rem
                 nx = tot - suf - n
             in if elem nx rs then
                    makeRing (prev ++ [(n, suf, nx)]) (delete nx rs) tot pre nx
                else [Nothing]

allRingsNToTot :: Int -> Int -> [[Trip]]
allRingsNToTot n tot = filter g (map (maybe [(0, 0, 0)] id) (filter isJust (foldl (++) [] rings)))
 where rings = map f [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n],
                                  a /= b, a /= c, b /= c,
                                  a + b + c == tot
                     ]
       f trip@(a, b, c) = makeRing [trip] (removeTrip trip [1..n]) tot b c
       removeTrip (a, b, c) ls = ((delete a) . (delete b) . (delete c)) ls
       g ts@((frst,_,_):_) = let gh _ [] = True 
                                 gh first ((h,_,_):ls) = if h < first then False
                                                         else gh first ls
                             in gh frst (tail ts)

allRingsN :: Int -> [[Trip]]
allRingsN n = foldl (++) [] (map (allRingsNToTot n) [6..(3 * (n - 2))])

tripToString :: [Trip] -> String 
tripToString [] = ""
tripToString ((a,b,c):ts) = show a ++ show b ++ show c ++ tripToString ts

p68 :: String
p68 = maximum $ filter (\s -> length s == 16) (map tripToString $ allRingsN 10)