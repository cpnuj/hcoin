{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HCoin.BlockCypherAPI where

import Data.Aeson
import Data.Proxy
import Data.Encoding (hexDecode)
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Control.Applicative
import HCoin.Data.Transaction

import qualified Data.Binary           as Binary
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSCH

newtype TxnResponse = TxnResponse { hex :: String }

instance FromJSON TxnResponse where
    parseJSON (Object v) = TxnResponse <$> v .: "hex"
    parseJSON _ = empty

-- https://api.blockcypher.com/v1/btc/test3/txs/18bc926d5c9824d9c5b0adc6718e8b1e28c028686a763be124f145e7b1003973?limit=50&includeHex=true
type TxnAPI = "v1" :> "btc" :> "test3" :> "txs"
-- type TxnAPI = "v1" :> "btc" :> "main" :> "txs"
           :> Capture "txs" String
           :> QueryParam "includeHex" Bool
           :> Get '[JSON] TxnResponse

txnCli :: String -> Maybe Bool -> ClientM TxnResponse
txnCli = client (Proxy :: Proxy TxnAPI)

run :: ClientM a -> IO a
run cli = do
    manager' <- newManager tlsManagerSettings
    res <- runClientM cli (mkClientEnv manager' (BaseUrl Https "api.blockcypher.com" 443 ""))
    case res of
        Left _  -> error "request fail"
        Right r -> return r

fetchTxn :: String -> IO Txn
fetchTxn txnid = do
    TxnResponse s <- run $ txnCli txnid (Just True)
    let x :: Txn
        x = Binary.decode (BS.fromStrict . hexDecode. BSCH.pack $ s)
    return x

