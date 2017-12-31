{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from RDRAND when available.

-}

module System.EntropyXen
        ( CryptHandle
        , openHandle
        , hGetEntropy
        , closeHandle
        , hardwardRNG
        ) where

import Control.Monad (liftM, when)
import Data.ByteString as B
import System.IO.Error (mkIOError, eofErrorType, ioeSetErrorString)

import Foreign (allocaBytes)
import Foreign.Ptr
import Foreign.C.Types
import Data.ByteString.Internal as B

#ifdef arch_i386
-- See .cabal wrt GCC 4.8.2 asm compilation bug
#undef HAVE_RDRAND
#endif

#ifndef HAVE_RDRAND
#error "The entropy package requires RDRAND support when using the halvm/Xen"
#endif
data CryptHandle = UseRdRand -- or die trying

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle = do
    b <- cpuHasRdRand
    if b then return UseRdRand
         else nonRDRandHandle
 where
  nonRDRandHandle :: IO CryptHandle
  nonRDRandHandle = error "entropy: On halvm there is no entropy other than RDRAND."

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle UseRdRand = return ()

-- | Get random values from the hardward RNG or return Nothing if no
-- supported hardware RNG is available.
--
-- Supported hardware:
--      * RDRAND
--      * Patches welcome
hardwareRandom :: Int -> IO (Maybe B.ByteString)
hardwareRandom sz = Just <$> hGetEntropy UseRdRand sz

-- |Read random data from a `CryptHandle`, which uses RDRAND (when on Xen)
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy UseRdRand = \n -> do
    B.create n $ \ptr ->  do
                r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                when (r /= 0)
                     (fail "RDRand failed to gather entropy")

foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
