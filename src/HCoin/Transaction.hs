module HCoin.Transaction where

import Data.Binary
import Data.Binary.Get
import Data.Encoding

import HCoin.Script

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS

data TxnInput = TxnInput
    { prevTxnID  :: BS.ByteString
    , prevTxnIdx :: Word32
    , scriptSig  :: Script
    , sequence   :: Word32
    }
    deriving Show

data TxnOutput = TxnOutput
    { amount       :: Word64
    , scriptPubKey :: Script
    }
    deriving Show

data Transaction = Transaction
    { txnVersion  :: Integer
    , txnInputs   :: [TxnInput]
    , txnOutputs  :: [TxnOutput]
    , txnLocktime :: Word32
    }
    deriving Show

getVersion :: Get Integer
getVersion =  bsToIntegerLE <$> getByteString 4

getVarint :: Get Integer
getVarint = getWord8 >>= \i -> case i of
    0xfd -> toInteger <$> getWord16le
    0xfe -> toInteger <$> getWord32le
    0xff -> toInteger <$> getWord64le
    _ -> return $ toInteger i

getVarBytes :: Get BS.ByteString
getVarBytes = getVarint >>= \len -> getByteString $ fromInteger len

getFields :: Get a -> Integer -> Get [a]
getFields _ 0 = return []
getFields getField n = do
    x  <- getField
    xs <- getFields getField (n-1)
    return $ x:xs

getInput :: Get TxnInput
getInput = TxnInput <$> getByteString 32
                    <*> getWord32le
                    <*> getScript
                    <*> getWord32le

getInputs :: Integer -> Get [TxnInput]
getInputs = getFields getInput

getOutput :: Get TxnOutput
getOutput = TxnOutput <$> getWord64le <*> getScript

getOutputs :: Integer -> Get [TxnOutput]
getOutputs = getFields getOutput

getCommand :: Get Command
getCommand = do
    b <- getWord8
    case b of
        76 -> getWord8 >>= getValue     -- OP_PUSHDATA1
        77 -> getWord16le >>= getValue  -- OP_PUSHDATA2
        x | 0x01 <= x && x <= 0x4b -> getValue x
          | otherwise -> return $ OP_CODE x
        where getValue n = do
                bs <- getByteString (fromIntegral n)
                return $ OP_PUSH $ Value bs

getCommands :: Get [Command]
getCommands = do
    empty <- isEmpty
    if empty
    then return []
    else do
        cmd <- getCommand
        remains <- getCommands
        return $ cmd : remains

decodeScript :: BS.ByteString -> Script
decodeScript bs = runGet getCommands $ LBS.fromStrict bs

getScript :: Get Script
getScript = do
    decodeScript <$> getVarBytes

getTxn :: Get Transaction
getTxn = Transaction
     <$> getVersion
     <*> (getVarint >>= \n -> getInputs n)
     <*> (getVarint >>= \n -> getOutputs n)
     <*> getWord32le

decodeTxn :: BS.ByteString -> Transaction
decodeTxn bs = runGet getTxn $ LBS.fromStrict bs
