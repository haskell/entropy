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

import Foreign (allocaBytes)
import Foreign.Ptr
import Foreign.C.Types
import Data.ByteString.Internal as B

#ifdef XEN
data CryptHandle = UseRdRand -- or die trying
#else

import System.Posix (openFd, closeFd, fdReadBuf, OpenMode(..), defaultFileFlags, Fd)

source :: FilePath
source = "/dev/urandom"

-- |Handle for manual resource mangement
data CryptHandle
    = CH Fd
#ifdef HAVE_RDRAND
    | UseRdRand
#endif
#endif

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle = do
#ifdef HAVE_RDRAND
    b <- cpuHasRdRand
    if b then return UseRdRand
         else nonRDRandHandle
#else
              nonRDRandHandle
#endif
 where
#ifdef XEN
  nonRDRandHandle :: IO CryptHandle
  nonRDRandHandle = error "entropy: On halvm there is no entropy other than RDRAND."
#else
  nonRDRandHandle :: IO CryptHandle
  nonRDRandHandle = liftM CH (openFd source ReadOnly Nothing defaultFileFlags)
#endif

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
#ifndef XEN
closeHandle (CH h) = closeFd h
#endif
#ifdef HAVE_RDRAND
closeHandle UseRdRand = return ()
#endif

-- |Read random data from a `CryptHandle`
#ifdef XEN
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy UseRdRand = \n -> do
    B.create n $ \ptr ->  do
                r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                when (r /= 0)
                     (fail "RDRand failed to gather entropy")
#else
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH h) = fdReadBS h
#ifdef HAVE_RDRAND
hGetEntropy UseRdRand = \n -> do
    B.create n $ \ptr ->  do
                r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                when (r /= 0)
                     (fail "RDRand failed to gather entropy")
#endif

fdReadBS :: Fd -> Int -> IO B.ByteString
fdReadBS fd n = do
    allocaBytes n $ \buf -> do
        rc <- fdReadBuf fd buf (fromIntegral n)
        case rc of
            0 -> ioError (ioeSetErrorString (mkIOError eofErrorType "fdRead" Nothing Nothing) "EOF")
            n' -> B.packCStringLen (castPtr buf, fromIntegral n')
#endif


#ifdef HAVE_RDRAND
foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
#endif

