module Convert where

import Bits

word_length = 32

b :: Int -> Bits
b = b' word_length

b' :: Int -> Int -> Bits
b' 0 _ = E
b' l x
  | x >= 0    = B (if mod x 2 == 0 then Zer else One) (b' (l-1) (div x 2))
  | otherwise = neg $ b' l (-x)

d :: Bits -> Int
d = d' word_length

d' :: Int -> Bits -> Int
d' 0 _          = 0
d' 1 (B One E)  = -1
d' n (B Zer bs) = 2 * d' (n-1) bs
d' n (B One bs) = 2 * d' (n-1) bs + 1
