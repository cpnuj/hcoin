module HCoin.Data.Transaction where

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Encoding

import HCoin.Data.Script
import HCoin.Data.Binary
import qualified Data.ByteString as BS

type TxnID = BS.ByteString

data TxnIn = TxnIn
    { prevTxnID  :: TxnID
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
        putByteString $ BS.reverse previd
        putWord32le idx
        putScript sig
        putWord32le seqn

    get = TxnIn <$> (BS.reverse <$> getByteString 32)
                <*> getWord32le
                <*> getScript
                <*> getWord32le

data TxnOut = TxnOut
    { amount       :: Word64
    , scriptPubKey :: Script
    }
    deriving Show

instance Binary TxnOut where
    put (TxnOut amt pubkey) = putWord64le amt >> putScript pubkey
    get = TxnOut <$> getWord64le <*> getScript

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
        putList' inputs
        putList' outputs
        putWord32le locktime

    get = Txn <$> getWord32le
              <*> (getVarint >>= \n -> getN n)
              <*> (getVarint >>= \n -> getN n)
              <*> getWord32le

getScript :: Get Script
getScript = decode . BS.fromStrict <$> getVarBytes

putScript :: Script -> Put
putScript = putVarBytes . BS.toStrict . encode

appendInput :: Txn -> TxnIn -> Txn
appendInput txn txnin =
    let inputs = txnInputs txn in
    txn { txnInputs = inputs <> [txnin] }

appendOutput :: Txn -> TxnOut -> Txn
appendOutput txn txnout =
    let outputs = txnOutputs txn in
    txn { txnOutputs = outputs <> [txnout] }

txnEmptyV1 :: Txn
txnEmptyV1 = Txn 1 [] [] 0

