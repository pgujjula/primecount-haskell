{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Math.NumberTheory.PrimeCount
-- Copyright   : 2021 Preetham Gujjula
-- License     : BSD3
-- Maintainer  : primecount-haskell@mail.preetham.io
-- Stability   : experimental
--
-- This module provides a high-level, polymorphic interface to the primecount
-- library. For a lower-level interface, see "Math.NumberTheory.PrimeCount.FFI#".
module Math.NumberTheory.PrimeCount
  ( primePi,
    nthPrime,
    primePhi,
  )
where

import Data.Int (Int64)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CSize)
import Foreign.Marshal.Array (allocaArray)
import Math.NumberTheory.PrimeCount.FFI
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- | The number of primes less than or equal to @n@. Throws an error if @n@
--   is too large. The maximum value for @n@ is is 10^31 on 64-bit systems and
--   2^63-1 on 32-bit systems. Also might throw an error if there's not enough
--   memory available to compute the result, which can happen even if @n@ is
--   smaller than the theoretical maximum.
primePi :: Integral a => a -> a
primePi n
  | n < 0 = 0
  | n' > bound = fromInteger (primePiStr n')
  | otherwise = fromIntegral (primecount_pi (fromInteger n'))
  where
    bound :: Integer
    bound = toInteger (maxBound :: Int64)

    n' :: Integer
    n' = toInteger n

primePiStr :: Integer -> Integer
primePiStr n = unsafePerformIO $ do
  withCString (show n) $ \nString -> do
    let len = 32 :: CSize
    allocaArray (fromIntegral len) $ \(res :: CString) -> do
      ret <- primecount_pi_str nString res len
      if ret < 0 || ret > fromIntegral len
        then error "primePi: call to primecount_pi_str failed"
        else do
          answer <- peekCString res
          maybe
            (error "primePi: couldn't parse result of primecount_pi_str")
            pure
            (readMaybe answer)

-- | The nth prime, starting at @nthPrime 1 == 2@.
--
--    * Throws an error if the input is less than 1.
--    * Throws an error if the input is larger than
--      @primePi ('maxBound' :: 'Int64') == 216289611853439384@.
nthPrime :: Integral a => a -> a
nthPrime n
  | n < 1 = error "nthPrime: n must be >= 1"
  | n' > bound = error "nthPrime: answer cannot be packed into a 64-bit int"
  | otherwise = fromIntegral (primecount_nth_prime (fromInteger n'))
  where
    bound :: Integer
    bound = 216289611853439384

    n' :: Integer
    n' = toInteger n

-- | @primePhi n a@ counts the number of positive integers @<= n@ that are not
--    divisible by any of the first @a@ primes. Throws an error if @n@ is larger
--    than @'maxBound' :: 'Int64'@.
primePhi :: Integral a => a -> a -> a
primePhi n a
  | n <= 0 = 0
  | a <= 0 = n
  | n' > bound = error "primePhi: input cannot be packed into a 64-bit int"
  | otherwise = fromIntegral (primecount_phi (fromInteger n') (fromInteger a'))
  where
    bound :: Integer
    bound = toInteger (maxBound :: Int64)

    n' :: Integer
    n' = toInteger n

    a' :: Integer
    a' = min bound (toInteger a)
