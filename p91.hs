
delta :: Float
delta = 0.00001

hpi :: Float
hpi = pi / 2

wn :: Float -> Float -> Bool
wn a b = abs (a - b) <= delta

arePQRight :: (Int, Int) -> (Int, Int) -> Bool
arePQRight (px, py) (qx, qy) = 
    let alpha = atan (fromIntegral qy / fromIntegral qx)
        beta  = atan (fromIntegral py / fromIntegral px)
        gamma = atan (fromIntegral (py - qy) / fromIntegral (qx - px))
    in wn hpi (pi - gamma - beta) ||
       wn hpi (alpha + gamma)

upperN :: Int
upperN = 50

validPQs :: [((Int, Int), (Int, Int))]
validPQs = [ ((px, py), (qx, qy)) | px <- [0..upperN],
                                    py <- [0..upperN],
                                    qx <- [0..upperN],
                                    qy <- [0..upperN],
                                    px < qx,
                                    py > qy,
                                    arePQRight (px, py) (qx, qy) ]

countTries :: Int
countTries = 3 * upperN * upperN + length validPQs
