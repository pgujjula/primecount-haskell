<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

# Changelog

## 0.1.0.1 (Revision 1)

### Updated
* Allow tasty-1.5.

## 0.1.0.1

### Updated
* CI now supports GHC versions 9.6.2, 9.4.5, 9.2.8, 9.0.2, and 8.10.7.

## 0.1.0.0

### Added
* `Math.NumberTheory.Prime.Count.FFI`, with FFI bindings to all functions from the
  `primecount` library.
* `Math.NumberTheory.Prime.Count`, with high-level wrappers (`primePi`,
  `nthPrime`, and `primePhi`) around the functions in
  `Math.NumberTheory.Prime.Count.FFI`.
* Test coverage of `primePi`, `nthPrime`, and `primePhi` with `tasty-hunit`.
* Benchmarks of `primePi`, `nthPrime`, and `primePhi` with `tasty-bench`.
* Full Haddock documentation coverage.
* Support for GHC 9.2.1, 9.0.1, 8.10.7, 8.8.4, 8.6.5, and 8.4.4, and
  libprimecount v7.x, verified by GitHub Actions.
