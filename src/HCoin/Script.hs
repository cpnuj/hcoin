module HCoin.Script where

import Control.Monad.State
import Control.Monad.Except
import Data.Binary (Word8)
import Data.ByteString ( ByteString )
import Data.Encoding
import Crypto.ECDSA (verify, decodePubSEC, decodeSig)

newtype Value = Value ByteString
              deriving Eq

instance Show Value where
    show (Value bs) = show $ bsToInteger bs

data Command = OP_PUSH Value
             | OP_CODE Word8
             deriving (Eq, Show)

type Script = [Command]

data Env = Env
    { envZ     :: Integer
    , envStack :: [Value]
    }
    deriving Show

type ScriptM = ExceptT String (State Env)

pop :: ScriptM Value
pop = do
    env <- get
    let stack = envStack env
    if null stack
    then throwError "pop on empty stack"
    else do
        put $ env { envStack = tail stack }
        return $ head stack

push :: Value -> ScriptM ()
push val = modify $ \env -> env { envStack = val:envStack env }

opDUP :: ScriptM ()
opDUP = do
    val <- pop
    push val
    push val

opHash256 :: ScriptM ()
opHash256 = do
    Value bs <- pop
    push $ Value (hash256 bs)

opCheckSig :: ScriptM ()
opCheckSig = do
    z <- gets envZ
    Value pubkey <- pop
    Value sig    <- pop

    if verify (decodePubSEC pubkey) z (decodeSig sig) then
        push $ Value (integerToBS 1)
    else
        push $ Value (integerToBS 0)

    return ()

evalCmd :: Command -> ScriptM ()
evalCmd (OP_PUSH v) = push v
evalCmd (OP_CODE 0xac) = opCheckSig
evalCmd _ = undefined

evalCommands :: [Command] -> ScriptM ()
evalCommands cmds = forM_ cmds evalCmd

runScript :: Integer -> Script -> Bool
runScript z cmds =
    let eval = runState $ runExceptT $ evalCommands cmds
        (res, env) = eval $ Env z []
    in
    case res of
        Left _ -> False
        _ -> case envStack env of
                [Value x] -> bsToIntegerLE x == 0
                _ -> False
