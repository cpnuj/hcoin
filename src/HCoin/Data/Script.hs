{-# LANGUAGE DeriveDataTypeable #-}

module HCoin.Data.Script where

import Control.Monad.Except
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.ByteString ( ByteString )
import Data.Encoding
import Data.Data
import Crypto.ECDSA (verify, decodePubSEC, decodeSig)

import HCoin.Data.Binary

import Control.Monad.State (runState)
import qualified Control.Monad.State as State

import qualified Data.ByteString as BS

type Value = ByteString

data Command = OP_PUSH Value
             | OP_DUP
             | OP_EqualVerify
             | OP_Hash160
             | OP_CheckSig
             deriving (Data, Typeable, Eq)

instance Show Command where
    show (OP_PUSH val)  = "OP_PUSH " <> show (hexEncode val)
    show x = show $ toConstr x

instance Binary Command where
    put OP_DUP         = putWord8 0x76
    put OP_EqualVerify = putWord8 0x88
    put OP_Hash160     = putWord8 0xa9
    put OP_CheckSig    = putWord8 0xac

    put (OP_PUSH val)  = do
        case BS.length val of
            x | 0x01 <= x && x <= 0x4b -> putWord8 (fromIntegral x)
              | 0x4c <= x && x <= 255  -> putWord8 76 >> putWord8 (fromIntegral x)
              | 256 <= x && x <= 65535 -> putWord8 77 >> putWord16le (fromIntegral x)
              | otherwise -> error "OP_PUSH value too long to encode"
        putByteString val

    get = do
        let getValue n = do
                bs <- getByteString (fromIntegral n)
                return $ OP_PUSH bs

        let opcode 0x4c = getWord8 >>= getValue     -- OP_PUSHDATA1
            opcode 0x4d = getWord16le >>= getValue  -- OP_PUSHDATA2
            opcode 0x76 = return OP_DUP
            opcode 0x88 = return OP_EqualVerify
            opcode 0xa9 = return OP_Hash160
            opcode 0xac = return OP_CheckSig
            opcode x | 0x01 <= x && x <= 0x4b = getValue x
                     | otherwise = error "invalid script code"

        getWord8 >>= opcode

newtype Script = Script [Command] deriving Show

instance Binary Script where
    put (Script cmds) = mapM_ put cmds
    get = Script <$> getTillEmpty

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
evalCmd (OP_PUSH v)    = push v
evalCmd OP_DUP         = opDUP
evalCmd OP_EqualVerify = opEqualVerify
evalCmd OP_Hash160     = opHash160
evalCmd OP_CheckSig    = opCheckSig

evalCommands :: Script -> ScriptM ()
evalCommands (Script cmds) = forM_ cmds evalCmd

p2pkhPubkey :: Value -> Script
p2pkhPubkey h160 = Script
    [ OP_DUP
    , OP_Hash160
    , OP_PUSH h160
    , OP_EqualVerify
    , OP_CheckSig
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
