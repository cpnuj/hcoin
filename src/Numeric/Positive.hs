{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Numeric.Positive
    ( PositiveLe(..)
    , PositiveBe(..)
    , Positive
    ) where

import Data.Binary
import Data.Bits
import Numeric.Natural

import Data.Binary.Get      (getRemainingLazyByteString)
import Data.ByteString.Lazy (unpack)
import Data.List            (unfoldr, foldl')

-- unroll unfold an Integer to a list of its bytes.
-- see https://hackage.haskell.org/package/binary-0.8.9.1/docs/src/Data.Binary.Class.html#unroll
unroll :: (Integral a, Bits a) => a -> [Word8]
unroll = unfoldr step
    where
        step 0 = Nothing
        step i = Just (fromIntegral i, i `shiftR` 8)

-- roll fold an Integer from a list of its bytes.
-- see https://hackage.haskell.org/package/binary-0.8.9.1/docs/src/Data.Binary.Class.html#roll
roll :: (Integral a, Bits a) => [Word8] -> a
roll = foldl' unstep 0 . reverse
    where
        unstep a b = a `shiftL` 8 .|. fromIntegral b

--
-- PositiveLe and PositiveBe is two newtypes of Natural used to implement
-- new Binary instances to encode and decode natural number compatible
-- with Bitcoin.
--

-- PositiveLe implements Binary instance for encoding and decoding natural
-- numbers in little endian
newtype PositiveLe = PositiveLe { leToNatural :: Natural }
    deriving ( Integral, Real, Num, Enum, Eq, Ord, Show, Read, Bits )

instance Binary PositiveLe where
    put n = mapM_ put $ unroll n
    get = do bytes <- unpack <$> getRemainingLazyByteString
             return $ PositiveLe (roll bytes)

-- PositiveBe implements Binary instance for encoding and decoding natural
-- numbers in big endian
newtype PositiveBe = PositiveBe { beToNatural :: Natural }
    deriving ( Integral, Real, Num, Enum, Eq, Ord, Show, Read, Bits )

instance Binary PositiveBe where
    put n = mapM_ put $ reverse (unroll n)
    get = do bytes <- reverse. unpack <$> getRemainingLazyByteString
             return $ PositiveBe (roll bytes)

type Positive = PositiveBe

