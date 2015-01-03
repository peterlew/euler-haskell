
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Map as M

upperN = 79

type Coord = (Int, Int)
type Node = (Maybe Int, Bool)
type Grid = [[Int]]

djikStep :: Coord -> Grid -> M.Map Coord Node -> M.Map Coord Node
djikStep (i, j) grid mp = 
    let cur = fromJust $ fst $ mp M.! (i, j)
        newMp = foldl (f cur) mp dirs 
    in M.insert (i, j) (Just cur, True) newMp
 where dirs = filter inRng [(i, j + 1), (i, j - 1), (i + 1, j), (i - 1, j)]
       inRng (a, b) = inRng' a && inRng' b
       inRng' x = x >= 0 && x <= upperN
       f c res (x, y) = let (d, vis) = res M.! (x, y) 
                        in if vis then res
                           else let n = (grid!!x)!!y
                                    potNew = c + n
                                in if isNothing d || potNew < fromJust d then 
                                    M.insert (x, y) (Just potNew, False) res
                                   else res

chooseNext :: M.Map Coord Node -> Maybe Coord
chooseNext m =
    let cands = M.filter isJust (M.map fst (M.filter f m))
    in if M.size cands == 0 then Nothing 
       else let ls = sort $ map swap (M.toList cands)
            in Just (snd (ls!!0))
 where f (_, v) = not v
       swap (a, b) = (b, a)

fillMap :: M.Map Coord Node -> Grid -> M.Map Coord Node
fillMap mp g = case chooseNext mp of 
                Nothing -> mp
                Just coords -> fillMap (djikStep coords g mp) g

readGrid :: [String] -> Grid
readGrid ls = map (map read) (map (splitOn ",") ls)

initMap :: Grid -> M.Map Coord Node
initMap g = M.insert (upperN, upperN) (Just ((g!!upperN)!!upperN), False) nothingMap
 where nothingMap = M.fromList $ zip cs (repeat (Nothing, False))
       cs = [ (x, y) | x <- [0..upperN], y <- [0..upperN]]

main = do
    handle <- readFile "assets/p83.txt"
    let ls = lines handle
        grid = readGrid ls
        mp = initMap grid
        filled = fillMap mp grid
    return (M.lookup (0, 0) filled)
