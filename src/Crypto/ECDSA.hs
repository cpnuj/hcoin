module Crypto.ECDSA
    ( sign
    , verify
    , genPubkey
    , SecKey
    , PubKey
    , Signature
    ) where

import Crypto.Math

-- SEC_p256k1 curve
curve :: Curve
curve = Curve pr a b
    where pr = 2 ^ (256::Int) - 2 ^ (32::Int) - 977
          a  = 0
          b  = 7

-- base generate point
pbase :: Point
pbase = Point
    0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
    0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8

-- order : n * pbase = pzero
n :: Integer
n  = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

type Signature = (Integer, Integer)

type SecKey = Integer

type PubKey = Point

genPubkey :: SecKey -> PubKey
genPubkey e = pscale curve e pbase

sign :: SecKey -> Integer -> Integer -> Signature
sign e k z = case pscale curve k pbase of
    PZero -> error "kG == zero point"
    Point x _ -> (r,s)
        where r = x `mod` n
              s = (z + r * e) * k_inv `mod` n
              k_inv = finv n k

verify :: PubKey -> Integer -> Signature -> Bool
verify PZero _ _ = False
verify p z (r,s) =
    let div = fdiv n
        u   = z `div` s
        v   = r `div` s
        ug  = pscale curve u pbase
        vp  = pscale curve v p
    in case padd curve ug vp of
        PZero -> False
        Point x _ -> x == r

