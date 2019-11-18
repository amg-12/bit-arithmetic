module Bits where

data Bit  = Zer | One      deriving Show
data Bits = E | B Bit Bits deriving Show

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

add :: Bit -> Bits -> Bits -> Bits
add _ E E               = E
add x ys E              = add x ys        (B Zer E)
add x E  zs             = add x (B Zer E) zs
add x (B y ys) (B z zs) = let r = adder x y z
                          in (B (fst r) (add (snd r) ys zs))

add' :: Bits -> Bits -> Bits
add' = add Zer

neg :: Bits -> Bits
neg = add' (B One E) . inv
  where inv :: Bits -> Bits
        inv E        = E
        inv (B x xs) = (B (not' x) (inv xs))

sub :: Bits -> Bits -> Bits
sub = (. neg) . add'
--sub x y = add' x $ neg y
