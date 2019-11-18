module Bits where

data Bit = Zer | One deriving Show

nand :: Bit -> Bit -> Bit
nand One One = Zer
nand _   _   = One

not' :: Bit -> Bit
not' x = nand x x

and' :: Bit -> Bit -> Bit
and' = (not' .) . nand
--and' x y = not' (nand x y)

or' :: Bit -> Bit -> Bit
--or' = (. not') . nand . not'
or' x y = nand (not' x) (not' y)

xor :: Bit -> Bit -> Bit
xor x y = and' (or' x y) (nand x y)

adder :: Bit -> Bit -> Bit -> (Bit, Bit)
adder x y z = (xor z (xor x y),
              or' (and' (xor x y) z) (and' x y))

add :: Bit -> [Bit] -> [Bit] -> [Bit]
add _ []     []     = []
add x ys     []     = add x ys    [Zer]
add x []     zs     = add x [Zer] zs
add x (y:ys) (z:zs) = let r = adder x y z
                      in [fst r] ++ add (snd r) ys zs

add' :: [Bit] -> [Bit] -> [Bit]
add' = add Zer

neg :: [Bit] -> [Bit]
neg = add' [One] . inv
  where inv :: [Bit] -> [Bit]
        inv []     = []
        inv (x:xs) = not' x : inv xs

sub :: [Bit] -> [Bit] -> [Bit]
sub = (. neg) . add'
--sub x y = add' x $ neg y
