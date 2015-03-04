
type Vec = (Double, Double)
type Point = (Double, Double)

getNormal :: Point -> Double
getNormal (x, y) = y / (4 * x)

normalize :: Vec -> Vec
normalize (x, y) = (x / d, y / d)
 where d = sqrt( x^2 + y^2 )

normVec :: Point -> Vec
normVec (x, y) = normalize $ norm (x, y)
 where norm (x, y) | x < 0 = (1, getNormal (x, y))
       norm (x, y) | x > 0 = (-1, -getNormal (x, y))

lineVec :: Point -> Point -> Vec
lineVec (x1, y1) (x2, y2) = normalize (x1 - x2, y1 - y2)

p1, p2 :: Point
p1 = (0, 10.1)
p2 = (1.4, -9.6)

dot :: Vec -> Vec -> Double
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

fmod :: Double -> Double -> Double
fmod x n | x >= 0 && x < n = x
fmod x n | x < 0 = fmod (x + n) n
fmod x n = fmod (x - n) n

--I kind of can't believe this works in all cases... I planned on going back
--and adding in cases where the reflection requires a clockwise rotation,
--but it mysteriously worked right out of the box
angOfRefl :: Point -> Point -> Double
angOfRefl p1 p2 = 2 * fmod (acos (dot (lineVec p1 p2) (normVec p2))) (2 * pi)

nxtVec :: Point -> Point -> Vec
nxtVec p1 p2 = let theta = angOfRefl p1 p2
                   v = lineVec p1 p2 
               in (dot (cos theta, -sin theta) v, dot (sin theta, cos theta) v)

nxtPtFromVec :: Point -> Vec -> Point
nxtPtFromVec (x0, y0) (vx, vy) = 
    let a = 4 * vx^2 + vy^2
        b = 8 * x0 * vx + 2 * y0 * vy
        c = 4 * x0^2 + y0^2 - 100
        k = (-b + sqrt( b^2 - 4 * a * c)) / (2 * a)
    in (x0 + k * vx, y0 + k * vy)

nxtPt :: Point -> Point -> Point
nxtPt p1 p2 = nxtPtFromVec p2 (nxtVec p1 p2)

allPts :: [Point]
allPts = p1 : p2 : zipWith nxtPt allPts (tail allPts)

p144 :: Int
p144 = length $ takeWhile f (tail allPts)
 where f (x, y) = x < -0.01 || x > 0.01 || y < 0