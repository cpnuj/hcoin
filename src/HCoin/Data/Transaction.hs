{-# LANGUAGE TemplateHaskell #-}

module HCoin.Data.Transaction where

import Control.Lens

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Encoding

import HCoin.Data.Script
import HCoin.Data.Binary
import qualified Data.ByteString as BS

getScript :: Get Script
getScript = decode . BS.fromStrict <$> getVarBytes

putScript :: Script -> Put
putScript = putVarBytes . BS.toStrict . encode

type TxnID = BS.ByteString

data TxnIn = TxnIn
    { _prevTxnID  :: TxnID
    , _prevTxnIdx :: Word32
    , _scriptSig  :: Script
    , _sequence   :: Word32
    }

makeLenses ''TxnIn

instance Show TxnIn where
    show (TxnIn previd idx sig seqn) =
           "{ prevTxnID = " <> show (hexEncode previd)
        <> ", prevTxnIdx = " <> show idx
        <> ", scriptSig = " <> show sig
        <> ", sequence = " <> show seqn
        <> " }"

instance Binary TxnIn where
    put (TxnIn previd idx sig seqn) = do
        putByteString $ BS.reverse previd
        putWord32le idx
        putScript sig
        putWord32le seqn

    get = TxnIn <$> (BS.reverse <$> getByteString 32)
                <*> getWord32le
                <*> getScript
                <*> getWord32le

data TxnOut = TxnOut
    { _amount       :: Word64
    , _scriptPubKey :: Script
    }
    deriving Show

makeLenses ''TxnOut

instance Binary TxnOut where
    put (TxnOut amt pubkey) = putWord64le amt >> putScript pubkey
    get = TxnOut <$> getWord64le <*> getScript

data Txn = Txn
    { _txnVersion  :: Word32
    , _txnInputs   :: [TxnIn]
    , _txnOutputs  :: [TxnOut]
    , _txnLocktime :: Word32
    }
    deriving Show

makeLenses ''Txn

instance Binary Txn where
    put (Txn ver inputs outputs locktime) = do
        putWord32le ver
        putList' inputs
        putList' outputs
        putWord32le locktime

    get = Txn <$> getWord32le
              <*> (getVarint >>= \n -> getN n)
              <*> (getVarint >>= \n -> getN n)
              <*> getWord32le

appendInput :: Txn -> TxnIn -> Txn
appendInput txn txnin = over txnInputs (++ [txnin]) txn

appendOutput :: Txn -> TxnOut -> Txn
appendOutput txn txnout = over txnOutputs (++ [txnout]) txn

txnEmptyV1 :: Txn
txnEmptyV1 = Txn 1 [] [] 0

