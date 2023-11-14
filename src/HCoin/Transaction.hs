{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module HCoin.Transaction where

import Control.Lens
import Control.Monad.State
import Control.Applicative

import Crypto.ECDSA

import Data.Aeson hiding (encode, decode)
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Encoding

import Data.Either (fromRight)
import Data.Maybe  (fromJust)

import Network.HTTP.Simple

import Numeric.Positive

import HCoin.Script
import HCoin.Data.Binary

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Base16      as Hex
import qualified Data.ByteString.Base16.Lazy as HexL

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
    deriving Show

makeLenses ''TxnIn

instance Binary TxnIn where

    put (TxnIn previd idx sig seqn) = do
        putByteString (BS.reverse rawid)
        putWord32le   idx
        putScript     sig
        putWord32le   seqn
        where rawid = fromRight "" (Hex.decode previd)

    get = TxnIn
      <$> (Hex.encode . BS.reverse <$> getByteString 32)
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
    , _txnTestnet  :: Bool
    }
    deriving Show

makeLenses ''Txn

setMainnet :: Txn -> Txn
setMainnet = set txnTestnet True

instance Binary Txn where

    put (Txn ver inputs outputs locktime _) = do
        putWord32le ver
        putList'    inputs
        putList'    outputs
        putWord32le locktime

    get  =  Txn
        <$> getWord32le
        <*> (getVarint >>= \n -> getN n)
        <*> (getVarint >>= \n -> getN n)
        <*> getWord32le
        -- default testnet
        <*> pure True

appendInput :: Txn -> TxnIn -> Txn
appendInput txn txnin = over txnInputs (++ [txnin]) txn

appendOutput :: Txn -> TxnOut -> Txn
appendOutput txn txnout = over txnOutputs (++ [txnout]) txn

txnEmptyV1 :: Txn
txnEmptyV1 = Txn 1 [] [] 0 True

-- | TxnResponse defines the response data of blockcypher transaction query api.
newtype TxnResponse = TxnResponse { hex :: String } deriving Show

instance FromJSON TxnResponse where
    parseJSON (Object v) = TxnResponse <$> v .: "hex"
    parseJSON _ = empty

-- | fetchTxn fetch txn info by txn id from blockcypher api.
--
--   sample api:
--   https://api.blockcypher.com/v1/btc/test3/txs/18bc926d5c9824d9c5b0adc6718e8b1e28c028686a763be124f145e7b1003973?limit=50&includeHex=true
--
--   for main net:
--   https://api.blockcypher.com/v1/btc/main/txs/18bc926d5c9824d9c5b0adc6718e8b1e28c028686a763be124f145e7b1003973?limit=50&includeHex=true
--
fetchTxn :: Bool -> TxnID -> IO Txn
fetchTxn testnet txnid = do

    let api = baseapi testnet <> BC.unpack txnid <> "?limit=50&includeHex=true"

    request  <- parseRequest api
    response <- httpJSON request

    let TxnResponse payload = getResponseBody response 

    return $ decode . BS.fromStrict . BC.pack $ payload

    where baseapi True  = "https://api.blockcypher.com/v1/btc/test3/txs/"
          baseapi False = "https://api.blockcypher.com/v1/btc/main/txs/"

-- | inputScriptPubkey fetch the locked script pubkey of given input
inputScriptPubkey :: Bool -> TxnIn -> IO Script
inputScriptPubkey testnet txnin = do
    txn <- fetchTxn testnet (txnin ^. prevTxnID)
    return $
        txn ^. txnOutputs
            ^? ix (txnin ^. prevTxnIdx . to fromIntegral)
            ^. to fromJust . scriptPubKey

sighashAll :: LBS.ByteString
sighashAll = fromRight "" . HexL.decode $ "01000000"

-- | sighash calculate the z value of a transaction input
sighash :: Txn -> Int -> IO Integer
sighash txn idx = do

    pubkey <- inputScriptPubkey
                (txn ^. txnTestnet)
                (txn ^. txnInputs ^? ix idx ^. to fromJust)

    let -- empty field scriptSig of all inputs, then set
        -- scriptSig of target input to its script pubkey
        txn' = txn & txnInputs . traverse . scriptSig .~ Script []
                   & txnInputs . ix idx .   scriptSig .~ pubkey

        h256 = hash256lazy $ encode txn' <> sighashAll

    return $ fromIntegral (decode h256 :: PositiveBe)

-- | signInput signs the given txn input with pubkey
signInput :: SecKey -> Txn -> Int -> IO Txn
signInput seckey txn idx = do
    z   <- sighash txn idx
    sig <- sign seckey z
    return $ txn & txnInputs . ix idx . scriptSig
                .~ p2pkhSig sig 1 (genPubkey seckey)

-- | verifyInput verifies the correctness of given txn input
verifyInput :: Txn -> Int -> IO Bool
verifyInput txn idx = do

    z   <- sighash txn idx
    pub <- inputScriptPubkey
            (txn ^. txnTestnet)
            (txn ^. txnInputs ^? ix idx ^. to fromJust)

    let sig = txn ^. txnInputs ^? ix idx ^. to fromJust . scriptSig

    case runScript z (sig <> pub) of
        Left e -> liftIO (print e) >> return False
        _      -> return True

{-
-- | TxnFetcher provides a cache to store known transactions.
data TxnFetcher = TxnFetcher
    { _fetchCache   :: M.Map TxnID Txn
    , _fetchTestnet :: Bool
    }

makeLenses ''TxnFetcher

type TxnFetcherT = StateT TxnFetcher IO

-- | fetchTxn can fetch transactions in the TxnFetcherT monad.
--   The given TxnID shoule be hex format.
fetchTxn :: TxnID -> TxnFetcherT Txn
fetchTxn txnid = do
    fetcher <- gets id

    case preview (fetchCache . ix txnid) fetcher of

        -- hits cache
        Just i -> return i

        -- cache miss, fetch txn from blockcypher and store at cache
        Nothing -> do
            txn <- liftIO $ fetchTxnResp (fetcher ^. fetchTestnet) txnid
            modify $ fetchCache . ix txnid .~ txn
            return txn

-- | fetchTxnOut fetch txn output by given Txn and idx of txnInputs
fetchTxnOut :: Txn -> Int -> TxnFetcherT TxnOut
fetchTxnOut txn idx = do
    prevtxn <- fetchTxn (txnin ^. prevTxnID)
    return $ prevtxn ^. txnOutputs ^? ix previdx ^. to fromJust
    where
        txnin   = txn   ^. txnInputs  ^? ix idx ^. to fromJust
        previdx = txnin ^. prevTxnIdx .  to fromIntegral

testCache :: M.Map TxnID Txn
testCache = M.singleton "18bc926d5c9824d9c5b0adc6718e8b1e28c028686a763be124f145e7b1003973" (Txn 1 [] [txnout1, txnout2] 0)
    where txnout1 = TxnOut 1295632 (decode. BS.fromStrict . hexDecode $ "76a914321b3b6248602f862249abdde4bdcc5aacde79c188ac")
          txnout2 = TxnOut 13931 (decode. BS.fromStrict . hexDecode $ "76a914bfdbfedde9cd85734e1fa1cedbd74f29fb72ff9c88ac")

testTxn :: Txn
testTxn = txn''
    where
        txnin = TxnIn
            (hexDecode "18bc926d5c9824d9c5b0adc6718e8b1e28c028686a763be124f145e7b1003973")
            1 (Script []) 0
        txnout1 = TxnOut 10000 (p2pkhPubkey $ h160FromAddress "mk1Kb2FJuJ9yvPQY3V6yeCgxR5eQNrVGnw")
        txnout2 = TxnOut 3000  (p2pkhPubkey $ h160FromAddress "my1QuFguxa5qMS3Ratpwan789R23dHpQee")

        txn   = appendInput txnEmptyV1 txnin
        txn'  = appendOutput txn txnout1
        txn'' = appendOutput txn' txnout2

testSighash :: IO Integer
testSighash = do
    let f :: TxnFetcherT Integer
        f = do txn <- fetchTxn "452c629d67e41baec3ac6f04fe744b4b9617f8f859c63b3002f8684e7a4fee03"
               sighash txn 0
    (i, _) <- runStateT f M.empty
    return i

testSignInput :: IO Txn
testSignInput = do
    let sec = SecKey 8675309
    let txn :: Txn
        txn = decode . BS.fromStrict $ hexDecode "010000000199a24308080ab26e6fb65c4eccfadf76749bb5bfa8cb08f291320b3c21e56f0d0d00000000ffffffff02408af701000000001976a914d52ad7ca9b3d096a38e752c2018e6fbc40cdf26f88ac80969800000000001976a914507b27411ccf7f16f10297de6cef3f291623eddf88ac00000000"
    (txn', _) <- runStateT (signInput sec txn 0) M.empty
    return txn'

-}

