-- |
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : primecount-haskell@mail.preetham.io
-- Stability   : experimental
module Main (main) where

import qualified Test.Math.NumberTheory.PrimeCount (tests)
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "" [Test.Math.NumberTheory.PrimeCount.tests]
