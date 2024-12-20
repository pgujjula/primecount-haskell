<!--
SPDX-FileCopyrightText: Copyright Preetham Gujjula
SPDX-License-Identifier: BSD-3-Clause
-->

# `primecount` for Haskell
[![CI](https://github.com/pgujjula/primecount-haskell/actions/workflows/ci.yml/badge.svg)](https://github.com/pgujjula/primecount-haskell/actions/workflows/ci.yml)
[![REUSE status](https://api.reuse.software/badge/github.com/pgujjula/primecount-haskell)](https://api.reuse.software/info/github.com/pgujjula/primecount-haskell)

This library provides Haskell bindings to Kim Walisch's
[primecount](https://github.com/kimwalisch/primecount) library.

## Build instructions
First follow the
[directions](https://github.com/kimwalisch/primecount#installation)
for installing `libprimecount`. As stated in the directions, if you are
installing through your system's package manager, make sure to get the
development version of the primecount package, which might have a name like
`primecount-devel`. The current version of the Haskell bindings supports any
version of `libprimecount >= 7.0`.

Then you can build the Haskell bindings with Stack or Cabal, and read the
documentation.
```
# Stack
stack build
stack test
stack haddock primecount --open

# Cabal
cabal update
cabal build
cabal test
cabal haddock  # and then open the documentation manually
```

### Building `libprimecount` from source
If you build and install the original `primecount` library from source, instead
of through a package manager, then you need to make sure that your Haskell build
system knows where to find it. For example, on Linux `libprimecount` might be
installed to `/usr/local/lib64`.

Then if using Stack, add the following lines to your `stack.yaml` or your global
`~/.stack/config.yaml`:
```
extra-lib-dirs:
- /usr/local/lib64
extra-include-dirs:
- /usr/local/include
```
If using Cabal, add the following lines to your `~/.cabal/config`:
```
extra-lib-dirs:
  /usr/local/lib64
extra-include-dirs:
  /usr/local/include
```
or pass
`--extra-lib-dirs=/usr/local/lib64 --extra-include-dirs=/usr/local/include`
as an argument to Cabal.

## Bugs
Report any bugs on the Github issue tracker, or by emailing
[libraries@mail.preetham.io](mailto:libraries@mail.preetham.io).
