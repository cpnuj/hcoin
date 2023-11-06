{-# LANGUAGE OverloadedStrings #-}

module HCoin.Transaction where

import Crypto.ECDSA
import Data.Binary (encode)
import Data.Encoding
import HCoin.Data.Script
import HCoin.Data.Transaction

import Control.Lens

import Control.Monad.State

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSCH
import qualified Data.Map              as M
import qualified HCoin.BlockCypherAPI  as API

type TxnFetcherT = StateT (M.Map TxnID Txn) IO

fetchTxn :: TxnID -> TxnFetcherT Txn
fetchTxn txnid = do
    -- fetch txn by txn id from api, and store result in state map
    let fetch :: TxnFetcherT Txn
        fetch = do
            txn <- liftIO $ fetchTxnByAPI txnid
            modify (M.insert txnid txn)
            return txn

    r <- gets (M.lookup txnid)
    maybe fetch return r

fetchTxnHex :: TxnID -> TxnFetcherT Txn
fetchTxnHex = fetchTxn . hexEncode

fetchTxnOut :: TxnID -> Int -> TxnFetcherT TxnOut
fetchTxnOut txnid idx = (!! idx) . txnOutputs <$> fetchTxn txnid

fetchTxnOutHex :: TxnID -> Int -> TxnFetcherT TxnOut
fetchTxnOutHex txnid = fetchTxnOut (hexEncode txnid)

fetchTxnByAPI :: TxnID -> IO Txn
fetchTxnByAPI = API.fetchTxn . BSCH.unpack

sighashAll :: BS.ByteString
sighashAll = hexDecode "01000000"

-- | sighash calculate the z value of a transaction input
sighash :: Txn -> Int -> TxnFetcherT Integer
sighash txn idx = do
    let txnin = txnInputs txn !! idx
    prevtxn <- fetchTxnHex (prevTxnID txnin)
    let utxo = txnOutputs prevtxn !! fromIntegral (prevTxnIdx txnin)
    let pubkey = scriptPubKey utxo

    let inputs   = txnInputs txn
    let inputs'  = map (\i -> i { scriptSig = Script [] }) inputs
    let inputs'' = inputs' & ix idx .~ txnin { scriptSig = pubkey }

    let txn' = txn { txnInputs = inputs'' }
    let bs = BS.toStrict (encode txn') <> sighashAll

    return $ bsToInteger (hash256 bs)

signInput :: SecKey -> Txn -> Int -> TxnFetcherT Txn
signInput seckey txn idx = do
    z   <- sighash txn idx
    liftIO $ print z
    sig <- liftIO $ sign seckey z
    let script = p2pkhSig sig (genPubkey seckey)
    let txnin  = txnInputs txn !! idx
    let inputs = txnInputs txn & ix idx .~ txnin { scriptSig = script }
    return $ txn { txnInputs = inputs }

verifyInput :: Txn -> Int -> TxnFetcherT Bool
verifyInput txn idx = do
    let txnin = txnInputs txn !! idx
    let sig = scriptSig txnin
    z <- sighash txn idx
    pubkey <- scriptPubKey <$> fetchTxnOutHex (prevTxnID txnin) (fromIntegral $ prevTxnIdx txnin)
    let combined = sig <> pubkey
    liftIO $ print combined
    case runScript z combined of
        Left e -> liftIO (print e) >> return False
        _ -> return True

testTxn :: Txn
testTxn = txn''
    where
        txnin = TxnIn
            (hexDecode "18bc926d5c9824d9c5b0adc6718e8b1e28c028686a763be124f145e7b1003973")
            1 (Script []) 0
        txnout1 = TxnOut 10000 (p2pkhPubkey $ h160FromAddress "mk1Kb2FJuJ9yvPQY3V6yeCgxR5eQNrVGnw")
        txnout2 = TxnOut 3900  (p2pkhPubkey $ h160FromAddress "my1QuFguxa5qMS3Ratpwan789R23dHpQee")

        txn   = appendInput txnEmptyV1 txnin
        txn'  = appendOutput txn txnout1
        txn'' = appendOutput txn' txnout2

