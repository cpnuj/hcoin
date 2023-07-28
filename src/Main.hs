{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Control.Monad

newtype FieldElement = FieldElement (Integer, Integer)

fieldElement :: Integer -> Integer -> Maybe FieldElement
fieldElement prime number
    | number < 0 || number >= prime = Nothing
    | otherwise = Just $ FieldElement (prime, number)

instance Show FieldElement where
    show (FieldElement (p, n)) = "f" <> show p <> " " <> show n

instance Num (Maybe FieldElement) where

    a + b = do
        FieldElement (p1, n1) <- a
        FieldElement (p2, n2) <- b
        guard $ p1 == p2
        fieldElement p1 $ (n1 + n2) `mod` p1

    a - b = a + negate b

    a * b = do
        FieldElement (p1, n1) <- a
        FieldElement (p2, n2) <- b
        guard $ p1 == p2
        fieldElement p1 $ (n1 * n2) `mod` p1

    negate a = a >>= \(FieldElement (p, n)) -> fieldElement p $ (-n) `mod` p

    abs = undefined

    signum = undefined

    fromInteger = undefined

instance Fractional (Maybe FieldElement) where

    recip a = do
        FieldElement (p, _) <- a
        a ^ (p - 2)

    fromRational = undefined

main :: IO ()
main = do
    putStrLn "h coin"
