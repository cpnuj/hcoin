{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit

import Data.Binary
import Crypto.ECDSA
import Data.Encoding
import Data.ByteString (ByteString)
import Control.Monad (forM_)
import Numeric.Positive
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16

simpleVerify :: Test
simpleVerify = TestCase
    ( do
        let s = "simple verify fail"

        let px = 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
        let py = 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34
        let p  = PubKey px py

        assertBool s
            (verify p
                0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
                (Signature 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4))

        assertBool s
            (verify p
                0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
                (Signature 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395
                          0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4
                ))
    )

simpleSign :: Test
simpleSign = TestCase
    ( do
        let sk = SecKey 12345
        let pk = genPubkey sk
        let z = decode . BS.fromStrict $ hash256 "Programming Bitcoin!" :: PositiveBe
        let (Signature r s) = signk 1234567890 sk (fromIntegral z)

        assertEqual "public key fail" pk
            (PubKey 0xf01d6b9018ab421dd410404cb869072065522bf85734008f105cf385a023a80f
                   0x0eba29d0f0c5408ed681984dc525982abefccd9f7ff01dd26da4999cf3f6a295)

        assertEqual "z fail" z 0x969f6056aa26f7d2795fd013fe88868d09c9f6aed96965016e1936ae47060d48
        assertEqual "r fail" r 0x2b698a0f0a4041b77e63488ad48c23e8e8838dd1fb7520408b121697b782ef22
        assertEqual "s fail" s 0x1dbc63bfef4416705e602a7b564161167076d8b20990a0f26f316cff2cb0bc1a
    )

encodeTestData :: [(Bool, Integer, ByteString)]
encodeTestData =
    -- uncompressed
    [ (False, 5000, "04ffe558e388852f0120e46af2d1b370f85854a8eb0841811ece0e3e03d282d57c315dc72890a4f10a1481c031b03b351b0dc79901ca18a00cf009dbdb157a1d10")
    , (False, 2018^(5::Integer), "04027f3da1918455e03c46f659266a1bb5204e959db7364d2f473bdf8f0a13cc9dff87647fd023c13b4a4994f17691895806e1b40b57f4fd22581a4f46851f3b06")
    , (False, 0xdeadbeef12345, "04d90cd625ee87dd38656dd95cf79f65f60f7273b67d3096e68bd81e4f5342691f842efa762fd59961d0e99803c61edba8b3e3f7dc3a341836f97733aebf987121")
    -- compressed
    , (True, 5001, "0357a4f368868a8a6d572991e484e664810ff14c05c0fa023275251151fe0e53d1")
    , (True, 2019^(5::Integer), "02933ec2d2b111b92737ec12f1c5d20f3233a0ad21cd8b36d0bca7a0cfa5cb8701")
    , (True, 0xdeadbeef54321, "0296be5b1292f6c856b3c5654e886fc13511462059089cdf9c479623bfcbe77690")
    ]

testEncodeSEC :: Test
testEncodeSEC = TestCase $
    forM_ encodeTestData $ \(compress, e, expect) -> do
        let s = "seckey = " <> show e
        let p = genPubkey (SecKey e)
        let o = Base16.encode $ encodePubKey compress p
        assertEqual s expect o
        assertEqual s p (decodePubKey $ hexDecode o)

encSigTestData :: [(Signature, ByteString)]
encSigTestData =
    [ ( Signature
            0x37206a0610995c58074999cb9767b87af4c4978db68c06e8e6e81d282047a7c6
            0x8ca63759c1157ebeaec0d03cecca119fc9a75bf8e6d0fa65c841c8e2738cdaec
      , "3045022037206a0610995c58074999cb9767b87af4c4978db68c06e8e6e81d282047a7c60221008ca63759c1157ebeaec0d03cecca119fc9a75bf8e6d0fa65c841c8e2738cdaec" )
    ]

testEncodeSig :: Test
testEncodeSig = TestCase $
    forM_ encSigTestData $ \(sig, expect) -> do
        let s = "signature = " <> show sig
        let o = Base16.encode . BS.toStrict $ encode sig
        assertEqual s expect o
        assertEqual s sig (decode . BS.fromStrict $ hexDecode o)

testAddressData :: [(Bool, Bool, Integer, ByteString)]
testAddressData =
    [ (True, False, 5002,              "mmTPbXQFxboEtNRkwfh6K51jvdtHLxGeMA")
    , (True, True,  2020^(5::Integer), "mopVkxp8UhXqRYbCYJsbeE1h1fiF64jcoH")
    , (False, True, 0x12345deadbeef,   "1F1Pn2y6pDb68E5nYJJeba4TLg2U7B6KF1") ]

testAddress :: Test
testAddress = TestCase $
    forM_ testAddressData $ \x@(testnet, compress, e, expect) -> do
        let pubkey = genPubkey $ SecKey e
        assertEqual (show x) expect (genAddress testnet compress pubkey)

testWalletData :: [(Bool, Bool, Integer, ByteString)]
testWalletData =
    [ (True, True, 5003, "cMahea7zqjxrtgAbB7LSGbcQUr1uX1ojuat9jZodMN8rFTv2sfUK")
    , (True, False, 2021^(5::Integer), "91avARGdfge8E4tZfYLoxeJ5sGBdNJQH4kvjpWAxgzczjbCwxic")
    , (False, True, 0x54321deadbeef, "KwDiBf89QgGbjEhKnhXJuH7LrciVrZi3qYjgiuQJv1h8Ytr2S53a") ]

testWallet :: Test
testWallet = TestCase $
    forM_ testWalletData $ \x@(testnet, compress, e, expect) -> do
        let seckey = SecKey e
        assertEqual (show x) expect (genWallet testnet compress seckey)

tests :: Test
tests = TestList [ TestLabel "simple verify" simpleVerify
                 , TestLabel "simple sign"   simpleSign
                 , TestLabel "encode sec"    testEncodeSEC
                 , TestLabel "encode sig"    testEncodeSig
                 , TestLabel "address"       testAddress
                 , TestLabel "wallet"        testWallet ]

main :: IO ()
main = runTestTTAndExit tests
