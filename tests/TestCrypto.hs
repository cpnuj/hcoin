{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit

import Helper
import Crypto.Math
import Crypto.ECDSA

simpleVerify :: Test
simpleVerify = TestCase
    ( do
        let s = "simple verify fail"

        let px = 0x887387e452b8eacc4acfde10d9aaf7f6d9a0f975aabb10d006e4da568744d06c
        let py = 0x61de6d95231cd89026e286df3b6ae4a894a3378e393e93a0f45b666329a0ae34
        let p  = Point px py

        assertBool s
            (verify p
                0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
                ( 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395
                , 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4
                ))

        assertBool s
            (verify p
                0xec208baa0fc1c19f708a9ca96fdeff3ac3f230bb4a7ba4aede4942ad003c0f60
                ( 0xac8d1c87e51d0d441be8b3dd5b05c8795b48875dffe00b7ffcfac23010d3a395
                , 0x68342ceff8935ededd102dd876ffd6ba72d6a427a3edb13d26eb0781cb423c4
                ))
    )

simpleSign :: Test
simpleSign = TestCase
    ( do
        let sk = 12345
        let pk = genPubkey sk
        let z = hashMsg "Programming Bitcoin!"
        let (r,s) = sign sk 1234567890 z

        assertEqual "public key fail" pk
            (Point 0xf01d6b9018ab421dd410404cb869072065522bf85734008f105cf385a023a80f
                   0x0eba29d0f0c5408ed681984dc525982abefccd9f7ff01dd26da4999cf3f6a295)

        assertEqual "z fail" z 0x969f6056aa26f7d2795fd013fe88868d09c9f6aed96965016e1936ae47060d48
        assertEqual "r fail" r 0x2b698a0f0a4041b77e63488ad48c23e8e8838dd1fb7520408b121697b782ef22
        assertEqual "s fail" s 0x1dbc63bfef4416705e602a7b564161167076d8b20990a0f26f316cff2cb0bc1a
    )

tests :: Test
tests = TestList [ TestLabel "simple verify" simpleVerify
                 , TestLabel "simple sign" simpleSign ]

main :: IO ()
main = runTestTTAndExit tests
