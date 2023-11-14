{-# LANGUAGE OverloadedStrings #-}

module Data.Encoding where

import Data.Bits
import Data.List (elemIndex)
import Data.Char (chr, ord)
import Data.String (fromString)
import Data.ByteString ( ByteString )
import Numeric (showIntAtBase)

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy  as LBS

-- | Double sha256 hash
hash256 :: ByteString -> ByteString
hash256 = SHA256.hash . SHA256.hash

hash256lazy :: LBS.ByteString -> LBS.ByteString
hash256lazy = BS.fromStrict . SHA256.hash . SHA256.hash . BS.toStrict

-- | sha256 followed by ripemd160 hash
hash160 :: ByteString -> ByteString
hash160 = RIPEMD160.hash . SHA256.hash

-- | Decode a big endian Integer from a bytestring
bsToInteger :: ByteString -> Integer
bsToInteger = BS.foldl f 0
    where f n c = toInteger (ord c) .|. shiftL n 8

-- | Encode Integer to a big endian bytestring
integerToBS :: Integer -> ByteString
integerToBS = encodeAtBase [(chr 0)..] (256::Integer)

-- | Decode a little endian Integer from a bytestring
bsToIntegerLE :: ByteString -> Integer
bsToIntegerLE = BS.foldl f 0 . BS.reverse
    where f n c = toInteger (ord c) .|. shiftL n 8

-- | Encode a little endian Integer from a bytestring
integerToBSLE :: Integer -> ByteString
integerToBSLE = BS.reverse . integerToBS

hexTable :: [Char]
hexTable = "0123456789abcdef"

hexRev :: Char -> Integer
hexRev c | c `elem` ['0'..'9'] = toInteger (ord c - ord '0')
         | c `elem` ['a'..'z'] = 10 + toInteger (ord c - ord 'a')
         | c `elem` ['A'..'Z'] = 10 + toInteger (ord c - ord 'A')
         | otherwise = error "invalid hex byte"

-- | Decode hex a bytestring to Integer
hexToInteger :: ByteString -> Integer
hexToInteger = BS.foldl f 0
    where f n c = hexRev c .|. shiftL n 4

-- | Encode a bytestring to its hex presentation
hexEncode :: ByteString -> ByteString
hexEncode bs
    | even $ BS.length s = s
    | otherwise = BS.cons '0' s -- padding
    where s = encodeAtBase hexTable (16::Integer) (bsToInteger bs)

-- | Decode a bytestring in hex format
hexDecode :: ByteString -> ByteString
hexDecode = integerToBS . hexToInteger

b58Table :: [Char]
b58Table = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58Rev :: Char -> Integer
b58Rev c = case elemIndex c b58Table of
            Just i -> fromIntegral i
            _ -> error "invalid base58 char"

b58ToInteger :: ByteString -> Integer
b58ToInteger = BS.foldl f 0
    where f n c = b58Rev c + n * 58

b58Encode :: ByteString -> ByteString
b58Encode s = BS.replicate pad '1' <> res
    where res = encodeAtBase b58Table (58::Integer) (bsToInteger s)
          pad = BS.length $ BS.takeWhile (== chr 0) s -- padding '1'

b58Decode :: ByteString -> ByteString
b58Decode = integerToBS . b58ToInteger

encodeAtBase :: Integral a => [Char] -> a -> a -> ByteString
encodeAtBase table base num = fromString $ showIntAtBase base mapf num ""
    where mapf i = table !! fromIntegral i

encodeInt256 :: Integer -> ByteString
encodeInt256 num = pad <> raw
    where raw = integerToBS num
          pad = BS.replicate (32 - BS.length raw) (chr 0)

decodeInt256 :: ByteString -> Integer
decodeInt256 = bsToInteger

