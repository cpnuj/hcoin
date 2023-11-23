{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}

module HCoin.Script
    ( Command(..) , Script(..)
    , p2pkhSig, p2pkhPubkey
    , runScript
    )
    where

import Control.Monad.Except
import Control.Monad.State (State, gets, modify, runState)

import Crypto.ECDSA

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Data
import Data.Either

import qualified Data.ByteString as BS
import qualified Data.Encoding   as Encoding

type Value = BS.ByteString

-- | Command is the script command to make a script.
data Command
    = OP_1NEGATE
    | OP_1  | OP_2  | OP_3  | OP_4
    | OP_5  | OP_6  | OP_7  | OP_8
    | OP_9  | OP_10 | OP_11 | OP_12
    | OP_13 | OP_14 | OP_15 | OP_16
    | OP_PUSH Value
    | OP_DUP
    | OP_EQUAL
    | OP_EQUALVERIFY
    | OP_HASH160
    | OP_CHECKSIG
    | OP_CHECKMULTISIG
    deriving (Data, Typeable, Eq)

instance Show Command where
    show (OP_PUSH val)  = "OP_PUSH " <> show (Encoding.hexEncode val)
    show x = show $ toConstr x

instance Binary Command where

    put OP_1NEGATE     = undefined

    put (OP_PUSH val)  = do
        case BS.length val of
            x | 0x01 <= x && x <= 0x4b -> putWord8 (fromIntegral x)
              | 0x4c <= x && x <= 255  -> putWord8 76 >> putWord8 (fromIntegral x)
              | 256 <= x && x <= 65535 -> putWord8 77 >> putWord16le (fromIntegral x)
              | otherwise -> error "OP_PUSH value too long to encode"
        putByteString val

    put OP_1              = putWord8 0x51
    put OP_2              = putWord8 0x52
    put OP_3              = putWord8 0x53
    put OP_4              = putWord8 0x54
    put OP_5              = putWord8 0x55
    put OP_6              = putWord8 0x56
    put OP_7              = putWord8 0x57
    put OP_8              = putWord8 0x58
    put OP_9              = putWord8 0x59
    put OP_10             = putWord8 0x5a
    put OP_11             = putWord8 0x5b
    put OP_12             = putWord8 0x5c
    put OP_13             = putWord8 0x5d
    put OP_14             = putWord8 0x5e
    put OP_15             = putWord8 0x5f
    put OP_16             = putWord8 0x60

    put OP_DUP            = putWord8 0x76
    put OP_EQUAL          = putWord8 0x87
    put OP_EQUALVERIFY    = putWord8 0x88
    put OP_HASH160        = putWord8 0xa9

    put OP_CHECKSIG       = putWord8 0xac
    put OP_CHECKMULTISIG  = putWord8 0xae


    get = getWord8 >>= go . fromIntegral where
       go op
           -- OP_PUSH
           | 0x01 <= op && op <= 0x4b =
               getByteString op >>= \bs -> return $ OP_PUSH bs

           -- OP_PUSHDATA1
           | op == 0x4c = do
               len <- fromIntegral <$> getWord8
               getByteString len >>= \bs -> return $ OP_PUSH bs

           -- OP_PUSHDATA2
           | op == 0x4d = do
               len <- fromIntegral <$> getWord16le
               getByteString len >>= \bs -> return $ OP_PUSH bs

           | op == 0x51 = return OP_1
           | op == 0x52 = return OP_2
           | op == 0x53 = return OP_3
           | op == 0x54 = return OP_4
           | op == 0x55 = return OP_5
           | op == 0x56 = return OP_6
           | op == 0x57 = return OP_7
           | op == 0x58 = return OP_8
           | op == 0x59 = return OP_9
           | op == 0x5a = return OP_10
           | op == 0x5b = return OP_11
           | op == 0x5c = return OP_12
           | op == 0x5d = return OP_13
           | op == 0x5e = return OP_14
           | op == 0x5f = return OP_15
           | op == 0x60 = return OP_16

           | op == 0x76 = return OP_DUP
           | op == 0x87 = return OP_EQUAL
           | op == 0x88 = return OP_EQUALVERIFY
           | op == 0xa9 = return OP_HASH160
           | op == 0xac = return OP_CHECKSIG
           | op == 0xae = return OP_CHECKMULTISIG

           | otherwise = error $ "invalid opcode " <> show op

-- | Script represents a smart contract consisting of many commands.
newtype Script = Script [Command] deriving Show

instance Semigroup Script where
    (Script cmds) <> (Script cmds') = Script $ cmds <> cmds'

instance Binary Script where
    put (Script cmds) = mapM_ put cmds
    get = Script <$> getCommands where
        getCommands = do
            empty <- isEmpty
            if empty then
                return []
            else
                liftM2 (:) get getCommands

-- | Env is the runtime environment for script.
data Env = Env
    { envZ     :: Integer
    , envStack :: [Value]
    }
    deriving Show

type ScriptT = ExceptT String (State Env)
type ScriptM = ScriptT ()

pop :: ScriptT Value
pop = do
    stack <- gets envStack
    when (null stack) $ throwError "pop on empty stack"
    modify $ \env -> env { envStack = tail stack }
    return $ head stack

popn :: Integer -> ScriptT [Value]
popn = go []
    where go xs 0 = return xs
          go xs i = pop >>= \x -> go (xs <> [x]) (i-1)

push :: Value -> ScriptM
push val = modify $ \env -> env { envStack = val:envStack env }

popnum :: ScriptT Integer
popnum = decode . BS.fromStrict <$> pop

pushnum :: Integer -> ScriptM
pushnum = push . BS.toStrict . encode

dup :: ScriptM
dup = do
    val <- pop
    push val
    push val

hash256 :: ScriptM
hash256 = do
    bs <- pop
    push $ Encoding.hash256 bs

hash160 :: ScriptM
hash160 = do
    pubkey <- pop
    push $ Encoding.hash160 pubkey

equalverify :: ScriptM
equalverify = do
    v1 <- pop
    v2 <- pop
    when (v1 /= v2) $ throwError "opEqualVerify fails"

checksig :: ScriptM
checksig = do
    z <- gets envZ
    pubkey <- pop
    sig    <- pop

    let der = BS.dropEnd 1 sig

    if verify z pubkey der then
        pushnum 1
    else
        pushnum 0

checkmultisig :: ScriptM
checkmultisig = do

    pubkeys <- popnum >>= \n -> popn n
    sigs    <- popnum >>= \m -> popn m

    void pop

    z <- gets envZ

    go z pubkeys sigs
    where
        go :: Integer -> [BS.ByteString] -> [BS.ByteString] -> ScriptM
        go _ [] _  = pushnum 1
        go _ _  [] = pushnum 0
        go z keys@(k:ks) (s:ss)
            | verify z k s = go z ks   ss
            | otherwise    = go z keys ss

-- | evalcmd evaluates Command to the specific scriptM monad operation.
evalcmd :: Command -> ScriptM

evalcmd OP_1NEGATE       = undefined

evalcmd OP_1             = undefined
evalcmd OP_2             = undefined
evalcmd OP_3             = undefined
evalcmd OP_4             = undefined
evalcmd OP_5             = undefined
evalcmd OP_6             = undefined
evalcmd OP_7             = undefined
evalcmd OP_8             = undefined
evalcmd OP_9             = undefined
evalcmd OP_10            = undefined
evalcmd OP_11            = undefined
evalcmd OP_12            = undefined
evalcmd OP_13            = undefined
evalcmd OP_14            = undefined
evalcmd OP_15            = undefined
evalcmd OP_16            = undefined

evalcmd (OP_PUSH v)      = push v

evalcmd OP_DUP           = dup
evalcmd OP_EQUAL         = undefined
evalcmd OP_EQUALVERIFY   = equalverify
evalcmd OP_HASH160       = hash160
evalcmd OP_CHECKSIG      = checksig
evalcmd OP_CHECKMULTISIG = checkmultisig

evalscript :: Script -> ScriptM
evalscript (Script cmds) = forM_ cmds evalcmd

p2pkhPubkey :: Value -> Script
p2pkhPubkey h160 = Script
    [ OP_DUP
    , OP_HASH160
    , OP_PUSH h160
    , OP_EQUALVERIFY
    , OP_CHECKSIG
    ]

p2pkhSig' :: Value -> Value -> Script
p2pkhSig' sig sec = Script [OP_PUSH sig, OP_PUSH sec]

p2pkhSig :: Signature -> Word8 -> PubKey -> Script
p2pkhSig sig hashtype pubkey = p2pkhSig'
    (BS.toStrict $ encode sig <> encode hashtype) (BS.toStrict $ encode pubkey)

runScript :: Integer -> Script -> Either String ()
runScript z cmds
      -- no error should happen while running script
    | isLeft res        = Left (fromLeft "" res)

      -- there should be only one elem on stack
    | length stack /= 1 = Left "there should be only 1 value after runScript"

      -- and the stack elem should be one
    | not (valid stack) = Left "invalid script"

      -- everything is ok
    | otherwise         = Right ()

    where (res, env) = runState (runExceptT $ evalscript cmds) (Env z [])
          stack = envStack env
          valid = (== (1 :: Integer)) . decode . BS.fromStrict . head

