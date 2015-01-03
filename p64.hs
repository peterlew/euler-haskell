
import Data.List
import qualified Data.Map as M

type FracPiece = (Int, Int, Int, Int)

fracStep :: FracPiece -> FracPiece
fracStep (i, r, b, d) = 
    let nd = div (r - b * b) d 
        nn = sqrt (fromIntegral r) + fromIntegral b
        nw = floor (nn / fromIntegral nd)
    in (nw, r, nd * nw - b, nd)

continuedFrac :: Int -> [FracPiece]
continuedFrac n = 
    let w = floor (sqrt $ fromIntegral n)
        cf f = f : cf (fracStep f)
    in cf (w, n, w, 1)

lengthOfFirstRep :: [FracPiece] -> Maybe Int
lengthOfFirstRep fracs =
    let lof [] _ _ = Nothing
        lof (f:fs) cur seen = case elemIndex f seen of 
                               Just ind -> Just (cur - ind)
                               Nothing -> lof fs (cur + 1) (seen ++ [f])
    in lof fracs 0 [] 

periodOfN :: Int -> Int
periodOfN n | isSquare n = 0
 where isSquare x = let sx = floor $ sqrt $ fromIntegral x
                    in sx * sx == n
periodOfN n = maybe 0 id (lengthOfFirstRep $ continuedFrac n)

p64 :: Int
p64 = length $ filter odd (map periodOfN [1..10000])

