
import Data.List

fracCycleLen :: Int -> Int 
fracCycleLen d = let fcl d u res = if d > u then fcl d (u * 10) (res ++ [u])
                                   else case elemIndex u res of 
                                         Just ind -> length res - ind
                                         Nothing -> let dig = div u d
                                                        rem = mod u d
                                                    in if rem == 0 then 0
                                                    else fcl d (rem * 10) (res ++ [u])
                 in fcl d 1 []

p26 :: Maybe Int
p26 = let lens = map fracCycleLen [1..999]
      in elemIndex (maximum lens) lens