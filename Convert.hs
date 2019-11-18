module Convert where

import Bits

word_length = 32

b :: Int -> [Bit]
b = b' word_length

b' :: Int -> Int -> [Bit]
b' 0 _ = []
b' l x 
  | x >= 0    = [if mod x 2 == 0 then Zer else One] ++ b' (l-1) (div x 2)
  | otherwise = neg $ b' l (-x)

d :: [Bit] -> Int
d = d' word_length

d' :: Int -> [Bit] -> Int
d' 0 _        = 0
d' 1 [One]    = -1
d' n (Zer:xs) = 2 * d' (n-1) xs
d' n (One:xs) = 2 * d' (n-1) xs + 1
d' _ _        = undefined

