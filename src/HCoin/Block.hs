{-# LANGUAGE OverloadedStrings #-}

module HCoin.Block where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Encoding
import Data.WideWord.Word256

import Data.Word (Word32)
import Numeric.Positive

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Hex
import Data.Either (fromRight)

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

    get  =  Block <$> getWord32le
        <*> getHexByteString32le <*> getHexByteString32le
        <*> getWord32le <*> getByteString 4 <*> getWord32be
        where getHexByteString32le = Hex.encode . BS.reverse <$> getByteString 32

blockID :: Block -> BS.ByteString
blockID = Hex.encode . BS.reverse . hash256 . BS.toStrict . encode

target :: BS.ByteString -> Word256
target bits = coe * 256 ^ (exp - 3)
    where exp = fromIntegral (BS.last bits)
          coe = fromIntegral (decode . BS.fromStrict $ BS.take 3 bits :: PositiveLe)
