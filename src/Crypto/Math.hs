module Crypto.Math where

import Data.Bits

feq :: Integer -> Integer -> Integer -> Bool
feq pr a b = a `mod` pr == b `mod` pr

fadd :: Integer -> Integer -> Integer -> Integer
fadd pr a b = (a + b) `mod` pr

fsub :: Integer -> Integer -> Integer -> Integer
fsub pr a b = (a - b) `mod` pr

fmul :: Integer -> Integer -> Integer -> Integer
fmul pr a b = (a * b) `mod` pr

fneg :: Integer -> Integer -> Integer
fneg pr a = (-a) `mod` pr

finv :: Integer -> Integer -> Integer
finv pr a = fastPow a (pr-2) pr

fdiv :: Integer -> Integer -> Integer -> Integer
fdiv pr a b = fmul pr a (finv pr b)

fpow :: Integer -> Integer -> Integer -> Integer
fpow pr a n = fastPow a n pr

fsqrt :: Integer -> Integer -> Integer
fsqrt pr a
    | pr `mod` 4 /= 3 = error "fsqrt only support prime p making p % 4 = 3"
    | otherwise = fpow pr a pow
        where (pow, _) = (pr + 1) `divMod` 4

fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base p m
    | p == 1 = mod base m
    | even p = fastPow base (div p 2) m ^ (2::Integer) `mod` m
    | otherwise = (fastPow base (div (p-1) 2) m ^ (2::Integer) * base) `mod` m

--
-- Elliptic Curve Point (a, b, x, y) : y^2 = x^3 + ax + b
--

data Curve = Curve Integer Integer Integer deriving (Eq, Show)

data Point = Point Integer Integer | PZero deriving (Eq, Show)

padd :: Curve -> Point -> Point -> Point
padd _ PZero p = p
padd _ p PZero = p
padd (Curve pr a _) (Point x1 y1) (Point x2 y2)
    -- vertical intersection line
    | x1 == x2 && y1 /= y2 = PZero
    | x1 == x2 && y1 == y2 && y1 == 0 `mul` x1 = PZero

    -- double point, use tangent
    | x1 == x2 && y1 == y2 =
        let s  = (3 `mul` (x1 `pow` 2) `add` a) `dvd` (2 `mul` y1)
            x3 = (s `pow` 2) `sub` (2 `mul` x1)
            y3 = s `mul` (x1 `sub` x3) `sub` y1
        in  Point x3 y3

    -- general case
    | otherwise =
        let s = (y2 `sub` y1) `dvd` (x2 `sub` x1)
            x3 = (s `pow` 2) `sub` x1 `sub` x2
            y3 = s `mul` (x1 `sub` x3) `sub` y1
        in  Point x3 y3

    -- operators in finite field with given prime
    where add = fadd pr
          sub = fsub pr
          mul = fmul pr
          dvd = fdiv pr -- divide
          pow = fpow pr

pscale :: Curve -> Integer -> Point -> Point
pscale c n p = go n p PZero
    where
        add = padd c
        go co cur acc
            | co == 0 = acc
            | co .&. 1 > 0 = go (co `shiftR` 1) (add cur cur) (add acc cur)
            | otherwise = go (co `shiftR` 1) (add cur cur) acc

-- | presolve returns the (even_res, odd_res) of x on a given curve
presolve :: Curve -> Integer -> (Integer, Integer)
presolve (Curve pr a b) x =
    if even w then
        (w, pr - w)
    else
        (pr - w, w)
    where v = (x `pow` 3) `add` (a `mul` x) `add` b
          w = fsqrt pr v
          add = fadd pr
          mul = fmul pr
          pow = fpow pr
