{-# LANGUAGE OverloadedStrings #-}

module HCoin.Block where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Decimal

import Data.Encoding
import Data.WideWord.Word256

import Data.Either (fromRight)
import Data.Word   (Word32)

import Numeric.Positive

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexLazy

data Block = Block
    { blkVersion    :: Word32
    , blkPrevBlk    :: BS.ByteString
    , blkMerkleRoot :: BS.ByteString
    , blkTimestamp  :: Word32
    , blkBits       :: BS.ByteString
    , blkNonce      :: Word32
    }
    deriving Show

instance Binary Block where

    put (Block version prevblk mroot ts bits nonce) = do
        putWord32le version
        putHexByteStringLe prevblk
        putHexByteStringLe mroot
        putWord32le ts
        putByteString bits
        putWord32be nonce
        where putHexByteStringLe = putByteString . BS.reverse . fromRight "" . Hex.decode

    get  =  Block
        <$> getWord32le             -- version
        <*> getHexByteString32le    -- previous block
        <*> getHexByteString32le    -- merkle root
        <*> getWord32le             -- timestamp
        <*> getByteString 4         -- bits
        <*> getWord32be             -- nonce
        where getHexByteString32le = Hex.encode . BS.reverse <$> getByteString 32

blockID :: Block -> LBS.ByteString
blockID = LBS.reverse . hash256lazy . encode

bitsToTarget :: BS.ByteString -> Word256
bitsToTarget bits = coe * 256 ^ (exp - 3)
    where exp = fromIntegral (LBS.last bits')
          coe = fromIntegral (decode $ LBS.take 3 bits' :: PositiveLe)
          bits' = BS.fromStrict bits

targetToBits :: Word256 -> BS.ByteString
targetToBits target = BS.reverse coe <> BS.singleton exp
    where
        bs   = BS.toStrict $ LBS.dropWhile (== 0) (encode target)
        size = fromIntegral (BS.length bs) :: Word8
        (coe, exp) =
            if BS.head bs > 0x7f then
                (BS.singleton 0 <> BS.take 2 bs, size + 1)
            else
                (BS.take 3 bs, size)

difficulty :: BS.ByteString -> Decimal
difficulty bits = 0xffff * 256 ^ (0x1d - 3) / fromIntegral (bitsToTarget bits)

adjustedDifficulty :: Block -> Block -> BS.ByteString
adjustedDifficulty blast bfirst = targetToBits tnew
    where
        twoweeks = 60*60*24*14 :: Word256
        interval = fromIntegral (blkTimestamp bfirst - blkTimestamp blast)

        timediff | interval > twoweeks * 4     = twoweeks * 4
                 | interval < twoweeks `div` 4 = twoweeks `div` 4
                 | otherwise = interval

        told = bitsToTarget (blkBits blast)
        tnew = told * timediff `div` twoweeks

-- | check block's proof-of-work
checkPOW :: Block -> Bool
checkPOW blk@Block { blkBits = bits } = pow < tar
    where pow = decode (blockID blk) :: PositiveBe
          tar = fromIntegral (bitsToTarget bits)

-- lastBlk :: Block
-- lastBlk = decode . fromRight "" . HexLazy.decode $
--     "000000203471101bbda3fe307664b3283a9ef0e97d9a38a7eacd8800000000000000000010c8aba8479bbaa5e0848152fd3c2289ca50e1c3e58c9a4faaafbdf5803c5448ddb845597e8b0118e43a81d3"
--
-- firstBlk :: Block
-- firstBlk = decode . fromRight "" . HexLazy.decode $
--     "02000020f1472d9db4b563c35f97c428ac903f23b7fc055d1cfc26000000000000000000b3f449fcbe1bc4cfbcb8283a0d2c037f961a3fdf2b8bedc144973735eea707e1264258597e8b0118e5f00474"
