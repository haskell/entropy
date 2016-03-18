{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from system sources or x86 RDRAND when available.

 Currently supporting:

    - Windows via CryptoAPI
    - *nix systems via @\/dev\/urandom@
       - Includes QNX
    - Xen (only when RDRAND is available)
-}

module System.Entropy
        ( getEntropy,
#if defined(isWindows)
         module System.EntropyWindows
        ) where
import System.EntropyWindows
#else
#ifdef XEN
         module System.EntropyXen
        ) where
import System.EntropyXen
#else
         module System.EntropyNix
        ) where
import System.EntropyNix
#endif
#endif

import qualified Data.ByteString as B
import Control.Exception (bracket)

-- |Get a specific number of bytes of cryptographically
-- secure random data using the system-specific facilities.
--
-- Use RDRAND if available and XOR with '/dev/urandom' on *nix and CryptAPI when on
-- Windows.  In short, this entropy is considered cryptographically secure
-- but not true entropy.
getEntropy :: Int -> IO B.ByteString
getEntropy = bracket openHandle closeHandle . flip hGetEntropy
