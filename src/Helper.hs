module Helper where

import Numeric
import Crypto.Hash.SHA256
import Data.ByteString ( ByteString )
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as BS

hash256 :: ByteString -> ByteString
hash256 = hash . hash

hashMsg :: ByteString -> Integer
hashMsg msg = z
    where (z, _) = head . readHex . BS.unpack . toLazyByteString . byteStringHex . hash256 $ msg

hex :: Integer -> String
hex a = "0x" <> showHex a ""
