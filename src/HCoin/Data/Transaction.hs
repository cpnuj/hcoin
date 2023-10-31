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
        putByteString previd
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
        putList' inputs
        putList' outputs
        putWord32le locktime

    get = Txn <$> getWord32le
              <*> (getVarint >>= \n -> getN n)
              <*> (getVarint >>= \n -> getN n)
              <*> getWord32le

-- | fetch ScriptPubkey for a txn input
-- TODO: unimplemented
fetchScriptPubkey :: TxnIn -> Script
fetchScriptPubkey _ = Script [OP_DUP]

data SigType = SigHashAll

sigTypeBS :: SigType -> BS.ByteString
sigTypeBS SigHashAll = BS.reverse . BS.toStrict $ encode (1 :: Word32)

-- | sigHash calculate the z value of a transaction input
sigHash :: Txn -> Int -> BS.ByteString
sigHash txn idx = hash256 bs
    where
        f :: Int -> TxnIn -> TxnIn
        f i txnin =
          if i == idx then
              txnin { scriptSig = fetchScriptPubkey txnin }
          else
              txnin { scriptSig = Script [] }

        inputs = zipWith f [0..] (txnInputs txn)
        txn' = txn { txnInputs = inputs }

        bs = BS.toStrict (encode txn') <> sigTypeBS SigHashAll
