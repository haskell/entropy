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

-- |Handle for manual resource mangement
data CryptHandle
    = CH Fd
#ifdef HAVE_RDRAND
    | UseRdRand Fd
#endif

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle = do
#ifdef HAVE_RDRAND
    b <- cpuHasRdRand
    if b then UseRdRand `fmap` nonRDRandHandle
         else CH `fmap` nonRDRandHandle
#else
              CH `fmap` nonRDRandHandle
#endif
 where
  nonRDRandHandle :: IO Fd
  nonRDRandHandle = openFd source ReadOnly Nothing defaultFileFlags

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle (CH h) = closeFd h
#ifdef HAVE_RDRAND
closeHandle (UseRdRand h) = closeFd h
#endif

-- |Read random data from a `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH h) = fdReadBS h
#ifdef HAVE_RDRAND
hGetEntropy (UseRdRand h) = \n ->
 do bsURandom <- fdReadBS h n
    bsRDRAND  <- B.create n $ \ptr ->  do
                  r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                  when (r /= 0)
                       (fail "RDRand failed to gather entropy")
    return $ B.pack $ B.zipWith xor bsURandom bsRDRAND
#endif

fdReadBS :: Fd -> Int -> IO B.ByteString
fdReadBS fd n = do
    allocaBytes n $ \buf -> do
        rc <- fdReadBuf fd buf (fromIntegral n)
        case rc of
            0 -> ioError (ioeSetErrorString (mkIOError eofErrorType "fdRead" Nothing Nothing) "EOF")
            n' -> B.packCStringLen (castPtr buf, fromIntegral n')

#ifdef HAVE_RDRAND
foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
#endif
