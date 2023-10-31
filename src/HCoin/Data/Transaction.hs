module HCoin.Data.Transaction where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Encoding

import HCoin.Data.Script
import HCoin.Data.Binary

import qualified Data.ByteString as BS

data TxnIn = TxnIn
    { prevTxnID  :: BS.ByteString
    , prevTxnIdx :: Word32
    , scriptSig  :: Script
    , sequence   :: Word32
    }

instance Show TxnIn where
    show (TxnIn previd idx sig seqn) =
           "{ prevTxnID = " <> show (hexEncode previd)
        <> ", prevTxnIdx = " <> show idx
        <> ", scriptSig = " <> show sig
        <> ", sequence = " <> show seqn
        <> " }"

instance Binary TxnIn where
    put (TxnIn previd idx sig seqn) = do
        put previd
        putWord32le idx
        put sig
        putWord32le seqn

    get = TxnIn <$> getByteString 32
                <*> getWord32le
                <*> get
                <*> getWord32le

data TxnOut = TxnOut
    { amount       :: Word64
    , scriptPubKey :: Script
    }
    deriving Show

instance Binary TxnOut where
    put (TxnOut amt pubkey) = putWord64le amt >> put pubkey
    get = TxnOut <$> getWord64le <*> get

data Txn = Txn
    { txnVersion  :: Word32
    , txnInputs   :: [TxnIn]
    , txnOutputs  :: [TxnOut]
    , txnLocktime :: Word32
    }
    deriving Show

instance Binary Txn where
    put (Txn ver inputs outputs locktime) = do
        putWord32le ver
        putList inputs
        putList outputs
        putWord32le locktime

    get = Txn <$> getWord32le
              <*> (getVarint >>= \n -> getN n)
              <*> (getVarint >>= \n -> getN n)
              <*> getWord32le

getN :: (Integral a, Binary b) => a -> Get [b]
getN 0 = return []
getN n = do
    x <- get
    xs <- getN (n - 1)
    return $ x:xs

