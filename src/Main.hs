{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Data.Bits

data FNum = FNum (Integer, Integer)
          | FScalar Integer
          deriving Eq

fieldElement :: Integer -> Integer -> FNum
fieldElement prime number
    | number < 0 = error "number < 0"
    | number >= prime = error "number >= prime"
    | otherwise = FNum (prime, number)

instance Show FNum where
    show (FNum (p, n)) = "f" <> show p <> " " <> show n
    show (FScalar s) = "f_scalar " <> show s

instance Num FNum where

    (FNum (p1, n1)) + (FNum (p2, n2))
        | p1 /= p2  = error "cannot add numbers from different fields"
        | otherwise = fieldElement p1 $ (n1 + n2) `mod` p1

    _ + _ = error "cannot add field scalar"

    a - b = a + negate b

    (FNum (p1, n1)) * (FNum (p2, n2))
        | p1 /= p2  = error "cannot mul numbers from different fields"
        | otherwise = fieldElement p1 $ (n1 * n2) `mod` p1

    (FNum (p, n)) * (FScalar s) = fieldElement p $ (s * n) `mod` p
    (FScalar s) * (FNum (p, n)) = fieldElement p $ (s * n) `mod` p

    _ * _ = error "cannot mul two field scalars"

    negate (FNum (p,n)) = fieldElement p $ (-n) `mod` p
    negate _ = error "cannot negate field scalar"

    abs = undefined

    signum = undefined

    fromInteger = FScalar

instance Fractional FNum where

    recip a@(FNum (p, _)) = a ^ (p - 2)
    recip _ = error "cannot recip field scalar"

    fromRational = undefined

data Position a = PInf | Position (a, a) deriving (Eq, Show)

data Point a = Point
    { getCurve :: (a, a)
    , getPos   :: Position a
    }
    deriving Eq

instance (Show a) => Show (Point a) where
    show (Point c p) = "p" <> show c <> ": " <> show p

ecPoint :: (Eq a, Fractional a) => (a, a) -> (a, a) -> Point a
ecPoint (a, b) (x, y)
    | y ^^ 2 /= x ^^ 3 + a * x + b = error "not on curve"
    | otherwise = Point { getCurve = (a, b) , getPos  = Position (x, y) }

ecInf :: (a, a) -> Point a
ecInf c = Point c PInf

instance (Eq a, Fractional a) => Semigroup (Point a) where
    p1 <> p2
        | getCurve p1 /= getCurve p2 = error "p1 and p2 not at the same curve"
        | otherwise = let curve@(a, _) = getCurve p1 in
            case (getPos p1, getPos p2) of
                (_, PInf) -> p1
                (PInf, _) -> p2
                (Position (x1, y1), Position (x2, y2)) ->
                    if x1 == x2 then
                        if y1 == y2 then
                            let s = (3 * x1 ^^ 2 + a) / (2 * y1)
                                x3 = s^^2 - 2*x1
                                y3 = s * (x1 - x3) - y1
                            in
                                ecPoint curve (x3, y3)
                        else
                            ecInf curve
                    else
                        let s = (y2 - y1) / (x2 - x1)
                            x3 = s ^^ 2 - x1 - x2
                            y3 = s * (x1 - x3) - y1
                        in
                            ecPoint curve (x3, y3)

pscalar :: (Eq a, Fractional a) => Integer -> Point a -> Point a
pscalar n p = go n p (ecInf (getCurve p))
    where go co cur acc
            | co == 0 = acc
            | co .&. 1 > 0 = go (co `shiftR` 1) (cur <> cur) (acc <> cur)
            | otherwise = go (co `shiftR` 1) (cur <> cur) acc

secp256k1 :: Point FNum
secp256k1 = curve (f gx, f gy)
    where p  = 2^256 - 2^32 - 977
          f  = fieldElement p
          curve = ecPoint (f 0, f 7)
          gx = 0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
          gy = 0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8
          -- n  = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

main :: IO ()
main = do
    putStrLn "h coin"
