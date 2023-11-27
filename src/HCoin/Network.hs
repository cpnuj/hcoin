{-# LANGUAGE OverloadedStrings #-}

module HCoin.Network () where

import Data.Word
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString as BS
import Control.Monad (when)
import Data.Encoding (hash256)

data Envelope = Envelope
    { netMagic           :: Word32
    , netCommand         :: BS.ByteString
    , netPayload         :: BS.ByteString
    }
    deriving Show

instance Binary Envelope where

    put e = do
        let size = BS.length (netPayload e)
        let checksum = BS.take 4 $ hash256 (netPayload e)
        putWord32be (netMagic e)
        putByteString (netCommand e)
        putWord32le (fromIntegral size)
        putByteString checksum
        putByteString (netPayload e)

    get = do
       magic    <- getWord32be
       cmd      <- BS.dropWhileEnd (== 0) <$> getByteString 12
       size     <- getWord32le
       checksum <- getByteString 4
       payload  <- getByteString (fromIntegral size)
       -- TODO: use binary-strict ?
       when (checksum /= BS.take 4 (hash256 payload)) (error "checksum fails")
       return $ Envelope magic cmd payload

