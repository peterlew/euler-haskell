
import Useful

isValidX :: Integer -> Bool
isValidX x =
    let k = x^2 + x
        negK = negate k
    in hasPosIntQuadRoot 3 (-1) negK && hasPosIntQuadRoot 4 (-2) negK