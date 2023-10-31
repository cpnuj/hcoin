module HCoin.Data.Binary where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import qualified Data.ByteString as BS

getVarint :: Get Word
getVarint = getWord8 >>= \i -> case i of
    0xfd -> fromIntegral <$> getWord16le
    0xfe -> fromIntegral <$> getWord32le
    0xff -> fromIntegral <$> getWord64le
    _ -> return $ fromIntegral i

putVarint :: Integral a => a -> Put
putVarint x
    | x < 0xfd = putWord8 (fromIntegral x)
    | x <= fromIntegral (maxBound :: Word16) = putWord8 0xfd >> putWord16le (fromIntegral x)
    | x <= fromIntegral (maxBound :: Word32) = putWord8 0xfe >> putWord32le (fromIntegral x)
    | x <= fromIntegral (maxBound :: Word64) = putWord8 0xff >> putWord64le (fromIntegral x)
    | otherwise = error "putVarint: value too big"

getVarBytes :: Get BS.ByteString
getVarBytes = getVarint >>= \len -> getByteString $ fromIntegral len

putVarBytes :: BS.ByteString -> Put
putVarBytes bs = putVarint (BS.length bs) >> putByteString bs

getTillEmpty :: Binary a => Get [a]
getTillEmpty = do
    empty <- isEmpty
    if empty then return []
    else do
        x <- get
        xs <- getTillEmpty
        return $ x:xs
