{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module HCoin.BlockCypherAPI where

import Data.Aeson
import Data.Proxy
import Network.HTTP.Client     (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Control.Applicative
import HCoin.Data.Transaction
import qualified Data.Binary as Binary
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSCH
import Data.Encoding (hexDecode)

data TxnOutputResponse = TxnOutputResponse
    { value  :: Int
    , script :: String
    }
    deriving Show

instance FromJSON TxnOutputResponse where
    parseJSON (Object v) = TxnOutputResponse <$> v .: "value" <*> v .: "script"
    parseJSON _ = empty

newtype TxnResponse = TxnResponse
    { outputs :: [TxnOutputResponse] } deriving Show

instance FromJSON TxnResponse where
    parseJSON (Object v) = TxnResponse <$> v .: "outputs"
    parseJSON _ = empty

-- https://api.blockcypher.com/v1/btc/test3/txs/18bc926d5c9824d9c5b0adc6718e8b1e28c028686a763be124f145e7b1003973?limit=50&includeHex=true
type TxnAPI = "v1" :> "btc" :> "test3" :> "txs"
           :> Capture "txs" String
           :> Get '[JSON] TxnResponse

resp2TxnOut :: TxnOutputResponse -> TxnOut
resp2TxnOut (TxnOutputResponse amt sig) = TxnOut amt' sig'
    where amt' = fromIntegral amt
          sig' = Binary.decode . BS.fromStrict . hexDecode . BSCH.pack $ sig

-- | FIXME: fully implemet
resp2Txn :: TxnResponse -> Txn
resp2Txn resp = Txn
    { txnVersion  = 0
    , txnInputs   = []
    , txnOutputs  = map resp2TxnOut $ outputs resp
    , txnLocktime = 0
    }

txnCli :: String -> ClientM TxnResponse
txnCli = client (Proxy :: Proxy TxnAPI)

run :: ClientM a -> IO a
run cli = do
    manager' <- newManager tlsManagerSettings
    res <- runClientM cli (mkClientEnv manager' (BaseUrl Https "api.blockcypher.com" 443 ""))
    case res of
        Left _  -> error "request fail"
        Right r -> return r

fetchTxn :: String -> IO Txn
fetchTxn txnid = resp2Txn <$> (run . txnCli $ txnid)

