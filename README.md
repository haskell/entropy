# Introduction

This package allows Haskell users to easily acquire entropy for use in critical
security applications by calling out to either windows crypto api, unix/linux's
`/dev/urandom`. Hardware RNGs (currently RDRAND, patches welcome) are supported
via the `hardwareRNG` function.

If you wish to obtain an XOR of the hardware and system RNG consider:

```
import           Data.Bits (xor)
import qualified Data.ByteString as B
import qualified Control.Exception as X

xorRNG sz = do hw  <- hardwareRNG sz
               h   <- openHandle
               sys <- hGetEntropy h `X.finally` closeHandle h
               pure $ B.pack $ B.zipWith xor hw sys
```

This package supports Windows, {li,u}nix, QNX, and has preliminary support for HaLVM.

Typically tested on Linux and OSX - testers are as welcome as patches.

[![Build Status](https://travis-ci.org/TomMD/entropy.svg?branch=master)](https://travis-ci.org/TomMD/entropy)
