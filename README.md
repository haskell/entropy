# Introduction

This package allows Haskell users to easily acquire entropy for use in critical
security applications by calling out to either windows crypto api, unix/linux's
`getrandom` and `/dev/urandom`. Hardware RNGs (currently RDRAND, patches
welcome) are supported via the `hardwareRNG` function.

## Quick Start

To simply get random bytes use `getEntropy`:

```
#!/usr/bin/env cabal
{- cabal:
    build-depends: base, entropy, bytestring
-}
import qualified Data.ByteString as BS
import           System.Entropy

main :: IO ()
main = print . BS.unpack =<< getEntropy 16
-- Example output: [241,191,215,193,225,27,121,244,16,155,252,41,131,38,6,100]
```

## Faster Randoms from Hardware

Most x86 systems include a hardware random number generator.  These can be
faster but require more trust in the platform:

```
import qualified Data.ByteString as B
import           System.Entropy

eitherRNG :: Int -> IO B.ByteString
eitherRNG sz = maybe (getEntropy sz) pure =<< getHardwareEntropy sz

main :: IO ()
main = print . B.unpack =<< eitherRNG 32
```

This package supports Windows, {li,u}nix, QNX, and has preliminary support for HaLVM.

Typically tested on Linux and OSX - testers are as welcome as patches.

[![Build Status](https://travis-ci.org/TomMD/entropy.svg?branch=master)](https://travis-ci.org/TomMD/entropy)
