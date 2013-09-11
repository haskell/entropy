{-# LANGUAGE CPP, ForeignFunctionInterface, BangPatterns, ScopedTypeVariables #-}
{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from system sources.
 Currently, windows and *nix systems with a @/dev/urandom@ are supported.
-}

module System.Entropy
	( getEntropy
	, CryptHandle
	, openHandle
	, hGetEntropy
	, closeHandle
	) where

import Control.Monad (liftM)
import Data.ByteString as B
import System.IO.Error (mkIOError, eofErrorType, ioeSetErrorString)
import System.Posix (openFd, closeFd, fdReadBuf, OpenMode(..), defaultFileFlags, Fd)
import Foreign (allocaBytes)

#if defined(isWindows)
{- C example for windows rng - taken from a blog, can't recall which one but thank you!
        #include <Windows.h>
        #include <Wincrypt.h>
        ...
        //
        // DISCLAIMER: Don't forget to check your error codes!!
        // I am not checking as to make the example simple...
        //
        HCRYPTPROV hCryptCtx = NULL;
        BYTE randomArray[128];

        CryptAcquireContext(&hCryptCtx, NULL, MS_DEF_PROV, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT);
        CryptGenRandom(hCryptCtx, 128, randomArray);
        CryptReleaseContext(hCryptCtx, 0);
-}

import Data.ByteString.Internal as BI
import Data.Int (Int32)
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types
import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (toBool)
import Foreign.Storable (peek)

#ifdef HAVE_RDRAND
foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
#endif

data CryptHandle
    = CH Word32
#ifdef HAVE_RDRAND
    | UseRdRand
#endif

-- Define the constants we need from WinCrypt.h 
msDefProv :: String
msDefProv = "Microsoft Base Cryptographic Provider v1.0"
provRSAFull :: Word32
provRSAFull = fromIntegral 1
cryptVerifyContext :: Word32
cryptVerifyContext = fromIntegral 0xF0000000

-- Declare the required CryptoAPI imports 
foreign import stdcall unsafe "CryptAcquireContextA"
   c_cryptAcquireCtx :: Ptr Word32 -> CString -> CString -> Word32 -> Word32 -> IO Int32
foreign import stdcall unsafe "CryptGenRandom"
   c_cryptGenRandom :: Word32 -> Word32 -> Ptr Word8 -> IO Int32
foreign import stdcall unsafe "CryptReleaseContext"
   c_cryptReleaseCtx :: Word32 -> Word32 -> IO Int32

cryptAcquireCtx :: IO Word32
cryptAcquireCtx = 
   alloca $ \handlePtr -> 
      withCString msDefProv $ \provName -> do
         stat <- c_cryptAcquireCtx handlePtr nullPtr provName (fromIntegral 1) (fromIntegral cryptVerifyContext)
         if (toBool stat)
            then peek handlePtr
            else fail "c_cryptAcquireCtx"

cryptGenRandom :: Word32 -> Int -> IO B.ByteString
cryptGenRandom h i = 
   BI.create i $ \c_buffer -> do
      stat <- c_cryptGenRandom (fromIntegral h) (fromIntegral i) c_buffer
      if (toBool stat)
         then return ()
         else fail "c_cryptGenRandom"

cryptReleaseCtx :: Word32 -> IO ()
cryptReleaseCtx h = do
   stat <- c_cryptReleaseCtx h 0
   if (toBool stat)
      then return ()
      else fail "c_cryptReleaseCtx"

-- |Inefficiently get a specific number of bytes of cryptographically
-- secure random data using the system-specific facilities.
--
-- This function will return zero bytes
-- on platforms without a secure RNG!
getEntropy :: Int -> IO B.ByteString
getEntropy n = do
#ifdef HAVE_RDRAND
    b <- cpuHasRdRand
    if b then hGetEntropy UseRdRand n
         else do
#endif
   h <- cryptAcquireCtx
   bs <- cryptGenRandom h n
   let !bs' = bs
   cryptReleaseCtx h
   return bs'

-- |Open a handle from which random data can be read
openHandle :: IO CryptHandle
openHandle = do
#ifdef HAVE_RDRAND
    b <- cpuHasRdRand
    if b then return UseRdRand
         else do
#endif
    liftM CH cryptAcquireCtx

-- |Close the `CryptHandle`
closeHandle (CH h) = cryptReleaseCtx h

-- |Read from `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString 
hGetEntropy (CH h) = cryptGenRandom h
#ifdef HAVE_RDRAND
hGetEntropy UseRdRand =
    B.create n $ \ptr ->  do
                r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                when (r /= 0)
                     (fail "RDRand failed to gather entropy")
#endif

#else
{- Not windows, assuming nix with a /dev/urandom -}
import Foreign.Ptr

source :: FilePath
source = "/dev/urandom"

-- |Handle for manual resource mangement
data CryptHandle
    = CH Fd
#ifdef HAVE_RDRAND
    | UseRdRand
#endif

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle = do
#ifdef HAVE_RDRAND
    b <- cpuHasRdRand
    if b then return UseRdRand
         else do
#endif
    liftM CH (openFd source ReadOnly Nothing defaultFileFlags)

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle (CH h) = closeFd h
#ifdef HAVE_RDRAND
closeHandle UseRdRand = return ()
#endif

fdReadBS :: Fd -> Int -> IO B.ByteString
fdReadBS fd n = do
    allocaBytes n $ \buf -> do
        rc <- fdReadBuf fd buf (fromIntegral n)
        case rc of
            0 -> ioError (ioeSetErrorString (mkIOError eofErrorType "fdRead" Nothing Nothing) "EOF")
            n' -> B.packCStringLen (castPtr buf, fromIntegral n')

-- |Read random data from a `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH h) = fdReadBS h
#ifdef HAVE_RDRAND
hGetEntropy UseRdRand = \n -> do
    B.create n $ \ptr ->  do
                r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                when (r /= 0)
                     (fail "RDRand failed to gather entropy")
#endif

#ifdef HAVE_RDRAND
foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
#endif

-- |Inefficiently get a specific number of bytes of cryptographically
-- secure random data using the system-specific facilities.
--
-- Use '/dev/urandom' on *nix and CryptAPI when on Windows.  In short,
-- this entropy is considered "cryptographically secure" but not true
-- entropy.
getEntropy :: Int -> IO B.ByteString
getEntropy n = do
    h <- openHandle
    e <- hGetEntropy h n
    closeHandle h
    return e
#endif
{- OS Test -}
