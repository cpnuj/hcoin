module Crypto.ECDSA
    ( sign
    , signk
    , verify
    , mkSecKey
    , genPubkey
    , encodeInt256
    , encPubSEC
    , encSig
    , SecKey (..)
    , PubKey (..)
    , Signature (..)
    ) where

import Helper
import Crypto.Math
import System.Random
import Data.Int (Int8, Int64)
import Data.Binary (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

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

data Signature = Signature Integer Integer deriving (Eq, Show)

newtype SecKey = SecKey Integer deriving (Eq, Show)

data PubKey = PubKey Integer Integer deriving (Eq, Show)

mkSecKey :: Integer -> SecKey
mkSecKey = SecKey . (`mod` n)

genPubkey :: SecKey -> PubKey
genPubkey (SecKey e) = case pscale curve e pbase of
    PZero -> error "zero pubkey"
    Point x y -> PubKey x y

encodeInt8 :: Integral a => a -> ByteString
encodeInt8 = BS.toStrict . encode . toInt8
    where toInt8 :: Integral a => a -> Int8
          toInt8 = fromIntegral

encodeInt256 :: Integer -> ByteString
encodeInt256 num = pad <> raw
    where raw = integerToBS num
          pad = BS.replicate (32 - BS.length raw) 0

-- encode pubkey to SEC format
encPubSEC :: Bool -> PubKey -> ByteString
-- uncompressed
encPubSEC False (PubKey x y) = BS.pack [0x04] <> encodeInt256 x <> encodeInt256 y
-- compressed
encPubSEC True (PubKey x y)
    | even y    = BS.pack [0x02] <> encodeInt256 x
    | otherwise = BS.pack [0x03] <> encodeInt256 x

-- encode integer to DER number format
encodeIntDER :: Integer -> ByteString
encodeIntDER x = if BS.head bs >= 0x80 then BS.pack [0x00] <> bs else bs
    where bs = BS.dropWhile (== 0x00) . encodeInt256 $ x

-- encode signature to DER format
encSig :: Signature -> ByteString
encSig (Signature r s) = hmark <> blen <> bs
    where hmark = BS.pack [0x30] -- head marker
          vmark = BS.pack [0x02] -- value marker
          rval  = encodeIntDER r
          sval  = encodeIntDER s
          rlen  = encodeInt8 (BS.length rval)
          slen  = encodeInt8 (BS.length sval)
          -- body and body length
          bs    = vmark <> rlen <> rval <> vmark <> slen <> sval
          blen  = encodeInt8 (BS.length bs)

-- sign signs z with seckey and a random k
sign :: SecKey -> Integer -> IO Signature
sign seckey z = do
    k <- (`mod` n) <$> (randomIO :: IO Integer)
    return $ signk k seckey z

-- signk signs z with given seckey and k
signk :: Integer -> SecKey -> Integer -> Signature
signk k (SecKey e) z = case pscale curve k pbase of
    PZero -> error "kG == zero point"
    Point x _ -> Signature r s
        where r = x `mod` n
              s = (z + r * e) * k_inv `mod` n
              k_inv = finv n k

verify :: PubKey -> Integer -> Signature -> Bool
verify (PubKey x y) z (Signature r s) =
    let div = fdiv n
        p   = Point x y
        u   = z `div` s
        v   = r `div` s
        ug  = pscale curve u pbase
        vp  = pscale curve v p
    in case padd curve ug vp of
        PZero -> False
        Point r' _ -> r' == r

