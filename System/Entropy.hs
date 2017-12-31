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
          getHardwareEntropy,
          CryptHandle,
          openHandle,
          hGetEntropy,
          closeHandle
        ) where
#if defined(isWindows)
import System.EntropyWindows
#else
#ifdef XEN
import System.EntropyXen
#else
import System.EntropyNix
#endif
#endif

import qualified Data.ByteString as B
import Control.Exception (bracket)

-- |Get a specific number of bytes of cryptographically
-- secure random data using the *system-specific* sources. 
-- (As of 0.4.  Verions <0.4 mixed system and hardware sources)
--
-- The returned random value is considered cryptographically secure but not true entropy.
--
-- On some platforms this requies a file handle which can lead to resource
-- exhaustion in some situations.
getEntropy :: Int               -- ^ Number of bytes
           -> IO B.ByteString
getEntropy = bracket openHandle closeHandle . flip hGetEntropy

-- |Get a specific number of bytes of cryptographically
-- secure random data using a supported *hardware* random bit generator.
--
-- If there is no hardware random number generator then @Nothing@ is returned.
-- If any call returns non-Nothing then it should never be @Nothing@ unless
-- there has been a hardware failure.
--
-- If trust of the CPU allows it and no context switching is important,
-- a bias to the hardware rng with system rng as fall back is trivial:
--
-- @
-- let fastRandom nr = maybe (getEntropy nr) pure =<< getHardwareEntropy nr
-- @
--
-- The old, @<0.4@, behavior is possible using @xor@ from 'Data.Bits':
--
-- @
-- let oldRandom nr =
--      do hwRnd  <- maybe (replicate nr 0) BS.unpack <$> getHardwareEntropy nr
--         sysRnd <- BS.unpack <$> getEntropy nr
--         pure $ BS.pack $ zipWith xor sysRnd hwRnd
-- @
--
-- A less maliable mixing can be accomplished by replacing `xor` with a
-- composition of concat and cryptographic hash.
getHardwareEntropy :: Int                       -- ^ Number of bytes
                   -> IO (Maybe B.ByteString)
getHardwareEntropy = hardwareRandom
