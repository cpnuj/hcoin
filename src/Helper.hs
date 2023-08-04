{-# LANGUAGE OverloadedStrings #-}

module Helper where

import Numeric
import Crypto.Hash.SHA256
import Data.Bits
import Data.Char (chr, ord)
import Data.String (fromString)
import Data.ByteString ( ByteString )
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LC

hash256 :: ByteString -> ByteString
hash256 = hash . hash

hashMsg :: ByteString -> Integer
hashMsg msg = z
    where (z, _) = head . readHex . LC.unpack . toLazyByteString . byteStringHex . hash256 $ msg

encodeHex :: ByteString -> ByteString
encodeHex = LC.toStrict . toLazyByteString . byteStringHex

hex :: Integer -> String
hex a = "0x" <> showHex a ""

fromHex' :: String -> Integer
fromHex' s = n
    where (n,_) = head . readHex $ s

fromHex :: ByteString -> Integer
fromHex bs = fromHex' (BS.unpack bs)

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
hexRev 'a' = 10
hexRev 'b' = 11
hexRev 'c' = 12
hexRev 'd' = 13
hexRev 'e' = 14
hexRev 'f' = 15
hexRev c | ord c < ord '0' || ord c > ord '9' = error "invalid hex byte"
         | otherwise = toInteger $ ord c - ord '0'

hexDecodeInt :: ByteString -> Integer
hexDecodeInt = BS.foldl f 0
    where f n c = hexRev c .|. shiftL n 4

hexEncode :: ByteString -> ByteString
hexEncode bs
    | even $ BS.length s = s
    | otherwise = BS.cons '0' s -- padding
    where s = encodeAtBase hexTable (16::Integer) (bsToInteger bs)

hexDecode :: ByteString -> ByteString
hexDecode = integerToBS . hexDecodeInt

b58Table :: [Char]
b58Table = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

b58Encode :: ByteString -> ByteString
b58Encode s = encodeAtBase b58Table (58::Integer) (bsToInteger s)

encodeAtBase :: Integral a => [Char] -> a -> a -> ByteString
encodeAtBase table base num = fromString $ showIntAtBase base mapf num ""
    where mapf i = table !! fromIntegral i
