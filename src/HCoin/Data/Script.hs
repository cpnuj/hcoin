module HCoin.Data.Script where

import Control.Monad.Except
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString ( ByteString )
import Data.Encoding
import Crypto.ECDSA (verify, decodePubSEC, decodeSig)

import HCoin.Data.Binary

import Control.Monad.State (runState)
import qualified Control.Monad.State as State

import qualified Data.ByteString as BS

type Value = ByteString

data Command = OP_PUSH Value
             | OP_CODE Word8
             deriving Eq

instance Show Command where
    show (OP_PUSH val)  = "OP_PUSH " <> show (hexEncode val)
    show (OP_CODE code) = "OP_CODE " <> show code

instance Binary Command where
    put (OP_CODE code) = put code
    put (OP_PUSH val)  = do
        case BS.length val of
            x | 0x01 <= x && x <= 0x4b -> putWord8 (fromIntegral x)
              | 0x4c <= x && x <= 255  -> putWord8 76 >> putWord8 (fromIntegral x)
              | 256 <= x && x <= 65535 -> putWord8 77 >> putWord16le (fromIntegral x)
              | otherwise -> error "OP_PUSH value too long to encode"
        putByteString val

    get = do
        b <- getWord8
        case b of
            76 -> getWord8 >>= getValue     -- OP_PUSHDATA1
            77 -> getWord16le >>= getValue  -- OP_PUSHDATA2
            x | 0x01 <= x && x <= 0x4b -> getValue x
              | otherwise -> return $ OP_CODE x
        where getValue n = do
                bs <- getByteString (fromIntegral n)
                return $ OP_PUSH bs

newtype Script = Script [Command] deriving Show

instance Binary Script where
    put (Script cmds) = putVarBytes $ foldMap (BS.toStrict . encode) cmds
    get = do
        bs <- BS.fromStrict <$> getVarBytes
        let cmds = runGet getTillEmpty bs
        return $ Script cmds

data Env = Env
    { envZ     :: Integer
    , envStack :: [Value]
    }
    deriving Show

type ScriptM = ExceptT String (State.State Env)

pop :: ScriptM Value
pop = do
    env <- State.get
    let stack = envStack env
    if null stack
    then throwError "pop on empty stack"
    else do
        State.put $ env { envStack = tail stack }
        return $ head stack

push :: Value -> ScriptM ()
push val = State.modify $ \env -> env { envStack = val:envStack env }

opDUP :: ScriptM ()
opDUP = do
    val <- pop
    push val
    push val

opHash256 :: ScriptM ()
opHash256 = do
    bs <- pop
    push $ hash256 bs

opHash160 :: ScriptM ()
opHash160 = undefined

opCheckSig :: ScriptM ()
opCheckSig = do
    z <- State.gets envZ
    pubkey <- pop
    sig    <- pop

    if verify (decodePubSEC pubkey) z (decodeSig sig) then
        push $ integerToBS 1
    else
        push $ integerToBS 0

    return ()

opEqualVerify :: ScriptM ()
opEqualVerify = do
    v1 <- pop
    v2 <- pop
    when (v1 /= v2) $ throwError "opEqualVerify fails"

evalCmd :: Command -> ScriptM ()
evalCmd (OP_PUSH v) = push v
evalCmd (OP_CODE 0x76) = opDUP
evalCmd (OP_CODE 0x88) = opEqualVerify
evalCmd (OP_CODE 0xa9) = opHash160
evalCmd (OP_CODE 0xac) = opCheckSig
evalCmd _ = undefined

evalCommands :: Script -> ScriptM ()
evalCommands (Script cmds) = forM_ cmds evalCmd

p2pkhPubkey :: Value -> Script
p2pkhPubkey h160 = Script
    [ OP_CODE 0x76
    , OP_CODE 0xa9
    , OP_PUSH h160
    , OP_CODE 0x88
    , OP_CODE 0xac
    ]

p2pkhSig :: Value -> Value -> Script
p2pkhSig sig sec = Script [OP_PUSH sig, OP_PUSH sec]

runScript :: Integer -> Script -> Either String ()
runScript z cmds =
    let eval = runState $ runExceptT $ evalCommands cmds
        (res, env) = eval $ Env z []
    in
    case res of
        Left e -> Left e
        _ -> case envStack env of
                [x] -> if bsToIntegerLE x == 0 then Right () else Left "invalid script"
                _ -> Left "there should be only 1 value after runScript"
