-- |
-- Module      : Math.NumberTheory.PrimeCount
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : primecount-haskell@mail.preetham.io
-- Stability   : experimental
--
-- This module provides direct access to the C API of the primecount library.
-- It's recommended that you use the higher-level interface in
-- "Math.NumberTheory.PrimeCount#".
--
-- Documentation adapted from the [C API reference]
-- (https://github.com/kimwalisch/primecount/blob/master/doc/libprimecount.md#c-api-reference).
module Math.NumberTheory.PrimeCount.FFI
  ( primecount_pi,
    primecount_pi_str,
    primecount_nth_prime,
    primecount_phi,
  )
where

import Data.Int (Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize (..))

-- | Count the number of primes @<= x@.
foreign import ccall unsafe "primecount_pi"
  primecount_pi :: Int64 -> Int64

-- | Count the number of primes <= x (supports 128-bit)
foreign import ccall unsafe "primecount_pi_str"
  primecount_pi_str :: CString -> CString -> CSize -> IO Int

-- | Find the @n@th prime e.g.: @primecount_nth_prime 25 == 97@.
foreign import ccall unsafe "primecount_nth_prime"
  primecount_nth_prime :: Int64 -> Int64

-- | @primecount_phi x a@ counts the numbers @<= x@ that are not divisible by
--   any of the first @a@ primes.
foreign import ccall unsafe "primecount_phi"
  primecount_phi :: Int64 -> Int64 -> Int64
