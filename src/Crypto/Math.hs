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

fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base 1 m = mod base m
fastPow base pow m | even pow = (fastPow base (div pow 2) m) ^ 2 `mod` m
                   | odd  pow = (fastPow base (div (pow-1) 2) m ^ 2 * base) `mod` m

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
        let s  = (3 `mul` (x1 `pow` 2) `add` a) `div` (2 `mul` y1)
            x3 = (s `pow` 2) `sub` (2 `mul` x1)
            y3 = s `mul` (x1 `sub` x3) `sub` y1
        in  Point x3 y3

    -- general case
    | otherwise =
        let s = (y2 `sub` y1) `div` (x2 `sub` x1)
            x3 = (s `pow` 2) `sub` x1 `sub` x2
            y3 = s `mul` (x1 `sub` x3) `sub` y1
        in  Point x3 y3

    -- operators in finite field with given prime
    where add = fadd pr
          sub = fsub pr
          mul = fmul pr
          div = fdiv pr
          pow = fpow pr

pscale :: Curve -> Integer -> Point -> Point
pscale c n p = go n p PZero
    where
        add = padd c
        go co cur acc
            | co == 0 = acc
            | co .&. 1 > 0 = go (co `shiftR` 1) (add cur cur) (add acc cur)
            | otherwise = go (co `shiftR` 1) (add cur cur) acc

