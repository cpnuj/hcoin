{-# LANGUAGE OverloadedStrings #-}

module Data.Encoding where

import Data.Bits
import Data.Char (chr, ord)
import Data.String (fromString)
import Data.ByteString ( ByteString )
import Numeric (showIntAtBase)

import qualified Crypto.Hash.SHA256 as SHA256
import qualified Crypto.Hash.RIPEMD160 as RIPEMD160
import qualified Data.ByteString.Char8 as BS

-- | Double sha256 hash
hash256 :: ByteString -> ByteString
hash256 = SHA256.hash . SHA256.hash

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
b58Rev = undefined

b58Encode :: ByteString -> ByteString
b58Encode s = BS.replicate pad '1' <> res
    where res = encodeAtBase b58Table (58::Integer) (bsToInteger s)
          pad = BS.length $ BS.takeWhile (== chr 0) s -- padding '1'

encodeAtBase :: Integral a => [Char] -> a -> a -> ByteString
encodeAtBase table base num = fromString $ showIntAtBase base mapf num ""
    where mapf i = table !! fromIntegral i
