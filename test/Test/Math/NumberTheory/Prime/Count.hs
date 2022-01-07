{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : primecount-haskell@mail.preetham.io
-- Stability   : experimental
module Test.Math.NumberTheory.Prime.Count (tests) where

import Control.Exception (SomeException, catch, evaluate)
import Data.Int (Int64)
import Math.NumberTheory.Prime.Count (nthPrime, primePhi, primePi)
import System.IO (stderr)
import System.IO.Silently (hSilence)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: TestTree
tests =
  testGroup
    "Math.NumberTheory.Prime.Count"
    [primePiTest, nthPrimeTest, primePhiTest]

throwsException :: a -> IO Bool
throwsException thunk =
  (hSilence [stderr] (evaluate thunk) >> pure False)
    `catch` (\(_ :: SomeException) -> pure True)

primePiTest :: TestTree
primePiTest =
  testGroup
    "primePi tests"
    [ testCase "works for negative inputs" $
        primePi (-1 :: Int) @?= 0,
      testCase "works for small inputs" $ do
        primePi (5 :: Int) @?= 3
        primePi (10 :: Int) @?= 4
        primePi (100 :: Int) @?= 25,
      testCase "throws error when input is too large" $ do
        let limit :: Integer
            limit = 10 ^ (31 :: Int) + 1
        exceptionThrown <- throwsException (primePi limit)
        exceptionThrown @?= True
    ]

nthPrimeTest :: TestTree
nthPrimeTest =
  testGroup
    "nthPrime"
    [ testCase "throws error for non-positive inputs" $ do
        throwsException (nthPrime (0 :: Int))
          >>= (@?= True)
        throwsException (nthPrime ((-1) :: Int))
          >>= (@?= True),
      testCase "works for small inputs" $ do
        nthPrime (1 :: Int) @?= 2
        nthPrime (5 :: Int) @?= 11
        nthPrime (25 :: Int) @?= 97,
      testCase "throws error when input is too large" $
        let limit :: Int64
            limit = 216289611853439384
         in throwsException (nthPrime (limit + 1))
              >>= (@?= True)
    ]

primePhiTest :: TestTree
primePhiTest =
  testGroup
    "primePhi"
    [ testCase "is 0 if n is not positive" $ do
        primePhi (0 :: Int) undefined @?= 0
        primePhi ((-1) :: Int) undefined @?= 0,
      testCase "is n if a is not positive" $ do
        primePhi (10 :: Int) 0 @?= 10
        primePhi (10 :: Int) (-1) @?= 10,
      testCase "works for small inputs" $ do
        primePhi (1 :: Int) 2 @?= 1
        primePhi (10 :: Int) 2 @?= 3
        primePhi (10 :: Int) 3 @?= 2
        primePhi (10 :: Int) 4 @?= 1
        primePhi (10 :: Int) 5 @?= 1,
      testCase "throws error if n is too large" $ do
        throwsException (primePhi (limit + 1) 2)
          >>= (@?= True)
        throwsException (primePhi (limit + 1) 3)
          >>= (@?= True),
      testCase "works even if a is too large" $
        primePhi 10 (limit + 1) @?= 1
    ]
  where
    limit :: Integer
    limit = toInteger (maxBound :: Int64)
