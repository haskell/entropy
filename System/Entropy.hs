{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from system sources or x86 RDRAND when available.

 Currently supporting:

    - Windows via CryptoAPO
    - *nix systems via @\/dev\/urandom@
    - QNX
    - Xen only when RDRAND is available.
-}

module System.Entropy
        ( getEntropy,
#if defined(isWindows)
         module System.EntropyWindows
        ) where
import System.EntropyWindows
#else
         module System.EntropyNix
        ) where
import System.EntropyNix
#endif

import qualified Data.ByteString as B

-- |Inefficiently get a specific number of bytes of cryptographically
-- secure random data using the system-specific facilities.
--
-- Use '/dev/urandom' on *nix and CryptAPI when on Windows.  In short,
-- this entropy is considered cryptographically secure but not true
-- entropy.
getEntropy :: Int -> IO B.ByteString
getEntropy n = do
    h <- openHandle
    e <- hGetEntropy h n
    closeHandle h
    return e
