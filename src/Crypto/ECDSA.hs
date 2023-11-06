module Crypto.ECDSA
    ( sign
    , signk
    , verify
    , mkSecKey
    , genPubkey
    , encodePubSEC
    , decodePubSEC
    , encodeSig
    , decodeSig
    , genAddress
    , h160FromAddress
    , genWallet
    , SecKey (..)
    , PubKey (..)
    , Signature (..)
    ) where

import Crypto.Math
import Data.Encoding
import System.Random
import Data.Binary
import Data.Binary.Get
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary.Put (putByteString, runPut)

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

-- | Signature is an ECDSA signature
--   Signature r s
data Signature = Signature Integer Integer deriving (Eq, Show)

-- | SecKey is an ECDSA private key
newtype SecKey = SecKey Integer deriving (Eq, Show)

-- | PubKey represents an ECDSA public key
--   PubKey x y, where (x,y) is a point on curve
data PubKey = PubKey Integer Integer deriving (Eq, Show)

mkSecKey :: Integer -> SecKey
mkSecKey = SecKey . (`mod` n)

genPubkey :: SecKey -> PubKey
genPubkey (SecKey e) = case pscale curve e pbase of
    PZero -> error "zero pubkey"
    Point x y -> PubKey x y

-- encode pubkey to SEC format
encodePubSEC :: Bool -> PubKey -> ByteString
-- uncompressed
encodePubSEC False (PubKey x y) = BS.pack [0x04] <> encodeInt256 x <> encodeInt256 y
-- compressed
encodePubSEC True (PubKey x y)
    | even y    = BS.pack [0x02] <> encodeInt256 x
    | otherwise = BS.pack [0x03] <> encodeInt256 x

getPubSEC :: Get PubKey
getPubSEC = do
    header <- getWord8
    r <- decodeInt256 <$> getByteString 32
    s <- case header of
            0x02 -> return $ fst $ presolve curve r -- even
            0x03 -> return $ snd $ presolve curve r -- odd
            0x04 -> decodeInt256 <$> getByteString 32
            _ -> error "invalid SEC form"
    return $ PubKey r s

decodePubSEC :: ByteString -> PubKey
decodePubSEC bs = runGet getPubSEC $ LBS.fromStrict bs

-- encode integer to DER number format
encodeIntDER :: Integer -> ByteString
encodeIntDER x = if BS.head bs >= 0x80 then BS.cons 0x00 bs else bs
    where bs = integerToBS x

-- | encode signature to DER format
encodeSig :: Signature -> ByteString
encodeSig s = LBS.toStrict $ runPut $ putSig s

putSig :: Signature -> Put
putSig (Signature r s) = do
    let marker = BS.pack [0x02]
    let rbs = encodeIntDER r
    let sbs = encodeIntDER s
    let rlen = fromIntegral $ BS.length rbs :: Word8
    let slen = fromIntegral $ BS.length sbs :: Word8
    let payload = marker <> BS.cons rlen rbs <> marker <> BS.cons slen sbs
    putWord8 0x30 -- header
    putWord8 $ fromIntegral $ BS.length payload
    putByteString payload

getSig :: Get Signature
getSig = do
    _ <- getWord8 -- start
    _ <- getWord8 -- blen
    _ <- getWord8 -- marker
    rlen <- getWord8
    r <- bsToInteger <$> getByteString (fromIntegral rlen)
    _ <- getWord8 -- marker
    slen <- getWord8
    s <- bsToInteger <$> getByteString (fromIntegral slen)
    return $ Signature r s

decodeSig :: ByteString -> Signature
decodeSig bs = runGet getSig $ LBS.fromStrict bs

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
    let dvd = fdiv n
        p   = Point x y
        u   = z `dvd` s
        v   = r `dvd` s
        ug  = pscale curve u pbase
        vp  = pscale curve v p
    in case padd curve ug vp of
        PZero -> False
        Point r' _ -> r' == r

-- | append checksum to given bytestring and encode in base58
checksumB58 :: ByteString -> ByteString
checksumB58 payload = b58Encode $ (payload <>) . BS.take 4 $ hash256 payload

type Address = ByteString

genAddress :: Bool -> Bool -> PubKey -> Address
genAddress test compressed pubkey = checksumB58 payload
    where prefix | test = BS.pack [0x6f] | otherwise = BS.pack [0x00]
          h160 = hash160 $ encodePubSEC compressed pubkey
          payload = prefix <> h160

-- | h160FromAddress get the hash160 of pubkey from an address
h160FromAddress :: Address -> ByteString
h160FromAddress addr
    | checksum /= BS.take 4 (hash256 payload) = error "bad address"
    | otherwise = h160
    where raw  = b58Decode addr
          h160 = BS.drop 1 payload      -- drop net byte from payload
          payload  = BS.dropEnd 4 raw
          checksum = BS.takeEnd 4 raw

genWallet :: Bool -> Bool -> SecKey -> ByteString
genWallet test compress (SecKey e) = checksumB58 payload
    where prefix | test = BS.pack [0xef] | otherwise = BS.pack [0x80]
          suffix | compress = BS.pack [0x01] | otherwise = mempty
          payload = prefix <> encodeInt256 e <> suffix
