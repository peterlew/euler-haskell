
import Useful
import Data.List

firstToNDigits :: [Integer] -> Int -> Maybe Integer
firstToNDigits [] _ = Nothing
firstToNDigits (x:xs) n = if length (show x) >= n then Just x
                          else firstToNDigits xs n