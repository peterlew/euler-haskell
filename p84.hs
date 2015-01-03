
import System.Random
import Data.Array.IO
import Control.Monad
import Data.List
import qualified Data.IntMap as M

type Board = (M.IntMap Int, Int, Int, [Int], [Int])
--spaces (with land count), position, double count, CH, CC

rollDice :: IO Int
rollDice = getStdRandom (randomR (1, 4))

roll :: IO (Int, Int)
roll = do
    r1 <- rollDice
    r2 <- rollDice
    return (r1, r2)

shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs

makeMove :: Board -> (Int, Int) -> Board
makeMove (b, pos, dbs, ch, cc) (r1, r2) = 
    let nPos = mod (pos + r1 + r2) 40
        nDbs = if r1 == r2 then dbs + 1 else 0
    in if nDbs == 3 || nPos == 30 then
        ((M.insertWith (+) 10 1 b), 10, 0, ch, cc)
    else if elem nPos chances then
          let nnPos = case ch!!0 of
                       0 -> 0
                       1 -> 10
                       2 -> 11
                       3 -> 24
                       4 -> 39
                       5 -> 5
                       6 -> goToNext nPos [5, 15, 25, 35, 45]
                       7 -> goToNext nPos [5, 15, 25, 35, 45]
                       8 -> goToNext nPos [12, 28, 52]
                       9 -> mod (nPos - 3) 40
                       otherwise -> nPos
          in ((M.insertWith (+) nnPos 1 b), nnPos, nDbs, tail ch ++ [ch!!0], cc)
         else if elem nPos chests then
               let nnPos = case cc!!0 of
                            0 -> 0
                            1 -> 10
                            otherwise -> nPos
               in ((M.insertWith (+) nnPos 1 b), nnPos, nDbs, ch, tail cc ++ [cc!!0])
              else ((M.insertWith (+) nPos 1 b), nPos, nDbs, ch, cc)
 where goToNext nPos ls = mod ((dropWhile (<= nPos) ls)!!0) 40
       chances = [7, 22, 36]
       chests = [2, 17, 33]

nRolls :: Int
nRolls = 1000000

main = do
    rs <- sequence $ replicate nRolls roll
    ch <- shuffle [0..15]
    cc <- shuffle [0..15]
    let (finB, _, _, _, _) = foldl makeMove (M.empty, 0, 0, ch, cc) rs
        percs = M.map (\n -> fromIntegral n / fromIntegral nRolls) finB
        sPercs = sort $ map swap (M.toList percs)
    return sPercs
 where swap (a, b) = (b, a)




