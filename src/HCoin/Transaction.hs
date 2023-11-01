module HCoin.Transaction where

import Crypto.ECDSA
import Data.Binary
import Data.Encoding
import HCoin.Data.Script
import HCoin.Data.Transaction

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSCH
import qualified HCoin.BlockCypherAPI  as API

fetchTxn :: BS.ByteString -> IO Txn
fetchTxn = API.fetchTxn . BSCH.unpack

-- | fetch ScriptPubkey for a txn input
--   TODO: unimplemented
fetchScriptPubkey :: TxnIn -> IO Script
fetchScriptPubkey _ = undefined

data SigType = SigHashAll

sigTypeBS :: SigType -> BS.ByteString
sigTypeBS SigHashAll = BS.reverse . BS.toStrict $ encode (1 :: Word32)

-- | sigHash calculate the z value of a transaction input
sigHash :: Txn -> Txn -> Int -> Integer
sigHash prevtxn txn idx = bsToInteger $ hash256 bs
    where
        previdx = prevTxnIdx $ txnInputs txn !! idx
        pubkey = scriptPubKey $ txnOutputs prevtxn !! fromIntegral previdx

        f :: Int -> TxnIn -> TxnIn
        f i txnin =
          if i == idx then
              txnin { scriptSig = pubkey }
          else
              txnin { scriptSig = Script [] }

        inputs = zipWith f [0..] (txnInputs txn)
        txn' = txn { txnInputs = inputs }

        bs = BS.toStrict (encode txn') <> sigTypeBS SigHashAll

signInput :: Txn -> Int -> SecKey -> IO Txn
signInput txn idx seckey = do
    let txnin = txnInputs txn !! idx
    prevtxn <- fetchTxn $ prevTxnID txnin
    let z = sigHash prevtxn txn idx
    sig <- sign seckey z
    undefined
