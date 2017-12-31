{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from system sources or x86 RDRAND when available.

-}

module System.EntropyNix
        ( CryptHandle
        , openHandle
        , hGetEntropy
        , closeHandle
        , hardwareRandom
        ) where

import Control.Monad (liftM, when)
import Data.ByteString as B
import System.IO.Error (mkIOError, eofErrorType, ioeSetErrorString)
import Data.Bits (xor)

import Foreign (allocaBytes)
import Foreign.Ptr
import Foreign.C.Types
import Data.ByteString.Internal as B

#ifdef arch_i386
-- See .cabal wrt GCC 4.8.2 asm compilation bug
#undef HAVE_RDRAND
#endif

import System.Posix (openFd, closeFd, fdReadBuf, OpenMode(..), defaultFileFlags, Fd)

source :: FilePath
source = "/dev/urandom"

-- |Handle for manual resource management
data CryptHandle
    = CH Fd

-- | Get random values from the hardward RNG or return Nothing if no
-- supported hardware RNG is available.
--
-- Supported hardware:
--      * RDRAND
--      * Patches welcome
hardwareRandom :: Int -> IO (Maybe B.ByteString)
#ifdef HAVE_RDRAND
hardwareRandom n =
 do b <- cpuHasRdRand
    if b
     then Just <$> B.create n (\ptr ->
                      do r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                         when (r /= 0) (fail "RDRand failed to gather entropy"))
     else pure Nothing
#else
hardwareRandom _ = pure Nothing
#endif

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle = do CH `fmap` nonRDRandHandle
 where
  nonRDRandHandle :: IO Fd
  nonRDRandHandle = openFd source ReadOnly Nothing defaultFileFlags

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle (CH h) = closeFd h

-- |Read random data from a `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH h) = fdReadBS h

fdReadBS :: Fd -> Int -> IO B.ByteString
fdReadBS fd n =
    allocaBytes n $ \buf -> go buf n
 where
 go buf 0   = B.packCStringLen (castPtr buf, fromIntegral n)
 go buf cnt  | cnt <= n = do
        rc <- fdReadBuf fd (plusPtr buf (n - cnt)) (fromIntegral cnt)
        case rc of
            0 -> ioError (ioeSetErrorString (mkIOError eofErrorType "fdRead" Nothing Nothing) "EOF")
            n' -> go buf (cnt - fromIntegral n')
 go _ _     = error "Impossible!  The count of bytes left to read is greater than the request or less than zero!"

#ifdef HAVE_RDRAND
foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
#endif
