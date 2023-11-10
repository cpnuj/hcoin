module Crypto.ECDSA
    ( mkSecKey, genPubkey, sign, signk, verify
    , encodePubKey, decodePubKey
    , genWallet, genAddress, h160FromAddress
    , SecKey (..), PubKey (..), CompressedPubKey(..), Signature (..)
    ) where

import Crypto.Math
import Data.Encoding
import System.Random
import Data.Binary
import Data.Binary.Get
import Data.ByteString (ByteString)
import Data.Binary.Put
import Data.WideWord
import Numeric.Positive
import qualified Data.ByteString as BS

-- | SEC_p256k1 curve
curve :: Curve
curve = Curve pr a b
    where pr = 2 ^ (256::Int) - 2 ^ (32::Int) - 977
          a  = 0
          b  = 7

-- | base generate point
pbase :: Point
pbase = Point
    0x79be667ef9dcbbac55a06295ce870b07029bfcdb2dce28d959f2815b16f81798
    0x483ada7726a3c4655da4fbfc0e1108a8fd17b448a68554199c47d08ffb10d4b8

-- | order : n * pbase = pzero
n :: Integer
n  = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

putWord256 :: Word256 -> Put
putWord256 = putLazyByteString . encode

getWord256 :: Get Word256
getWord256 = decode <$> getLazyByteString 32

-- | encode integer to DER number format
encodeIntDER :: Integer -> ByteString
encodeIntDER x
    | BS.head bs >= 0x80 = BS.cons 0x00 bs
    | otherwise = bs
    where bs = BS.toStrict $ encode (fromIntegral x :: PositiveBe)

-- | Signature r s is an ECDSA signature
data Signature = Signature Integer Integer deriving (Eq, Show)

instance Binary Signature where

    put (Signature r s) = do
        let marker = BS.pack [0x02]
        let rbs = encodeIntDER r
        let sbs = encodeIntDER s
        let rlen = fromIntegral $ BS.length rbs :: Word8
        let slen = fromIntegral $ BS.length sbs :: Word8
        let payload = marker <> BS.cons rlen rbs <> marker <> BS.cons slen sbs
        putWord8 0x30 -- header
        putWord8 $ fromIntegral $ BS.length payload
        putByteString payload

    get = do
        _ <- getWord8 -- start
        _ <- getWord8 -- blen

        _ <- getWord8 -- marker
        rlen <- fromIntegral <$> getWord8
        r <- decode <$> getLazyByteString rlen :: Get PositiveBe

        _ <- getWord8 -- marker
        slen <- fromIntegral <$> getWord8
        s <- decode <$> getLazyByteString slen :: Get PositiveBe

        return $ Signature (fromIntegral r) (fromIntegral s)

-- | SecKey is an ECDSA private key
newtype SecKey = SecKey Integer deriving (Eq, Show)

-- | PubKey x y represents an ECDSA public key, where (x,y) is a point on curve
data PubKey = PubKey Integer Integer deriving (Eq, Show)

instance Binary PubKey where

    -- put uncompressed format of PubKey, for compressed format, see
    -- CompressedPubKey
    put (PubKey x y) = do
        putWord8 0x04
        putWord256 $ fromIntegral x
        putWord256 $ fromIntegral y

    get = do
        header <- getWord8
        r <- fromIntegral <$> getWord256
        s <- case header of
                0x02 -> return $ fst $ presolve curve r -- even
                0x03 -> return $ snd $ presolve curve r -- odd
                0x04 -> fromIntegral <$> getWord256
                _ -> error "invalid SEC form"
        return $ PubKey r s

newtype CompressedPubKey = CompressedPubKey { unPubkey :: PubKey } deriving (Eq, Show)

instance Binary CompressedPubKey where

    put p | even y    = putWord8 0x02 >> putWord256 (fromIntegral x)
          | otherwise = putWord8 0x03 >> putWord256 (fromIntegral x)
        where PubKey x y = unPubkey p

    get = CompressedPubKey <$> (get :: Get PubKey)

-- | encode pubkey to SEC format
encodePubKey :: PubKey -> Bool -> ByteString
encodePubKey pubkey False = BS.toStrict (encode pubkey)
encodePubKey pubkey True  = BS.toStrict (encode $ CompressedPubKey pubkey)

-- | decode pubkey from SEC format
decodePubKey :: ByteString -> PubKey
decodePubKey = decode . BS.fromStrict

-- | sign signs z with seckey and a random k
sign :: SecKey -> Integer -> IO Signature
sign seckey z = do
    k <- (`mod` n) <$> (randomIO :: IO Integer)
    return $ signk k seckey z

-- | signk signs z with given seckey and k
signk :: Integer -> SecKey -> Integer -> Signature
signk k (SecKey e) z = case pscale curve k pbase of
    PZero -> error "kG == zero point"
    Point x _ -> Signature r s
        where r = x `mod` n
              s = (z + r * e) * k_inv `mod` n
              k_inv = finv n k

-- | verify signature and pubkey
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

mkSecKey :: Integer -> SecKey
mkSecKey = SecKey . (`mod` n)

genPubkey :: SecKey -> PubKey
genPubkey (SecKey e) = case pscale curve e pbase of
    PZero -> error "zero pubkey"
    Point x y -> PubKey x y

-- | append checksum to given bytestring and encode in base58
checksumB58 :: ByteString -> ByteString
checksumB58 payload = b58Encode $ (payload <>) . BS.take 4 $ hash256 payload

type Address = ByteString

genAddress :: Bool -> Bool -> PubKey -> Address
genAddress testnet compress pubkey =
    let prefix  | testnet   = BS.singleton 0x6f
                | otherwise = BS.singleton 0x00
        h160    = hash160 $ encodePubKey pubkey compress
        payload = prefix <> h160
    in
        checksumB58 payload

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
