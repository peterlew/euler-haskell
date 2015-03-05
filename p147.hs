
fitsNormal :: Int -> Int -> Int -> Int -> Int
fitsNormal dw dh rw rh = (rw - dw + 1) * (rh - dh + 1)

fitsCrooked :: Int -> Int -> Int -> Int -> Int
fitsCrooked dw dh rw rh = -- bottom corner on a grid corner
                          (max 0 (rw - div (dw + 1) 2 - div (dh - 1) 2)) * 
                          (max 0 (rh - div (dw + dh - 1) 2)) + 
                          -- bottom corner in the middle of a square
                          (max 0 (rw - div dw 2 - div dh 2)) *
                          (max 0 (rh - div (dw + dh) 2))


fitsIn :: Int -> Int -> Int
fitsIn w h = sum [fitsNormal x y w h | x <- [1..w], y <- [1..h]] +
             sum [max 0 (fitsCrooked x y w h) |
                  x <- [1..2*w],
                  y <- [1..2*h]]

totalUnder :: Int -> Int -> Int
totalUnder w h = sum [fitsIn x y | x <- [1..w], y <- [1..h]]