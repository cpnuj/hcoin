{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Lazy.Char8  as LBC
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
setMainnet = set txnTestnet False

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
newtype TxnResponse = TxnResponse { hex :: LBS.ByteString } deriving Show

instance FromJSON TxnResponse where
    parseJSON (Object v) = TxnResponse . LBC.pack <$> v .: "hex"
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

    return $ decode . fromRight "" . HexL.decode $ payload

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

-- | isCoinbase checks the transaction is a coinbase transaction or not.
isCoinbase :: Txn -> Bool
isCoinbase txn = txn ^. txnInputs . to length == 1
              && input ^. prevTxnID  == BS.replicate 32 0
              && input ^. prevTxnIdx == 0xffffff
              where
                 input = txn ^. txnInputs ^? ix 0 ^. to fromJust

