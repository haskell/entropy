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

import Control.Exception
import Control.Monad (liftM, when)
import Data.ByteString as B
import System.IO.Error (mkIOError, eofErrorType, ioeSetErrorString)
import System.IO.Unsafe
import Data.Bits (xor)

import Foreign (allocaBytes)
import Foreign.Ptr
import Foreign.C.Error
import Foreign.C.Types
import Data.ByteString.Internal as B

#ifdef arch_i386
-- See .cabal wrt GCC 4.8.2 asm compilation bug
#undef HAVE_RDRAND
#endif

import System.Posix (openFd, closeFd, fdReadBuf, OpenMode(..), defaultFileFlags, Fd, OpenFileFlags(..))

source :: FilePath
source = "/dev/urandom"

-- |Handle for manual resource management
data CryptHandle
    = CH Fd
#ifdef HAVE_GETRANDOM
    | UseGetRandom
#endif

-- | Get random values from the hardware RNG or return Nothing if no
-- supported hardware RNG is available.
--
-- Supported hardware:
--      * RDRAND
--      * Patches welcome
hardwareRandom :: Int -> IO (Maybe B.ByteString)
#ifdef HAVE_RDRAND
hardwareRandom n =
 do b <- cpuHasRdRand
    if b then Just <$> B.create n (\ptr ->
                      do r <- c_get_rand_bytes (castPtr ptr) (fromIntegral n)
                         when (r /= 0) (fail "RDRand failed to gather entropy"))
     else pure Nothing
#else
hardwareRandom _ = pure Nothing
#endif

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle =
#ifdef HAVE_GETRANDOM
  if systemHasGetRandom then return UseGetRandom else
#endif
  fmap CH openRandomFile

openRandomFile :: IO Fd
openRandomFile = do
  evaluate ensurePoolInitialized
#if MIN_VERSION_unix(2,8,0)
  openFd source ReadOnly defaultFileFlags { creat = Nothing }
#else
  openFd source ReadOnly Nothing defaultFileFlags
#endif

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle (CH h) = closeFd h
#ifdef HAVE_GETRANDOM
closeHandle UseGetRandom = return ()
#endif

-- |Read random data from a `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH h) n = fdReadBS h n
#ifdef HAVE_GETRANDOM
hGetEntropy UseGetRandom n = do
  bs <- B.createUptoN n (\ptr -> do
    r <- c_entropy_getrandom (castPtr ptr) (fromIntegral n)
    return $ if r == 0 then n else 0)
  if B.length bs == n then return bs
  -- getrandom somehow failed. Fall back on /dev/urandom instead.
  else bracket openRandomFile closeFd $ flip fdReadBS n
#endif

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

#ifdef HAVE_GETRANDOM
foreign import ccall unsafe "system_has_getrandom"
   c_system_has_getrandom :: IO CInt
foreign import ccall safe "entropy_getrandom"
  c_entropy_getrandom :: Ptr CUChar -> CSize -> IO CInt

-- NOINLINE and unsafePerformIO are not totally necessary as getrandom will be
-- consistently either present or not, but it is cheaper not to check multiple
-- times.
systemHasGetRandom :: Bool
{-# NOINLINE systemHasGetRandom #-}
systemHasGetRandom = unsafePerformIO $ fmap (/= 0) c_system_has_getrandom
#endif

foreign import ccall safe "ensure_pool_initialized"
   c_ensure_pool_initialized :: IO CInt

-- Similarly to systemHasGetRandom, NOINLINE is just an optimization.
ensurePoolInitialized :: CInt
{-# NOINLINE ensurePoolInitialized #-}
ensurePoolInitialized = unsafePerformIO $ throwErrnoIfMinus1 "ensurePoolInitialized" $ c_ensure_pool_initialized

#ifdef HAVE_RDRAND
foreign import ccall unsafe "cpu_has_rdrand"
   c_cpu_has_rdrand :: IO CInt

foreign import ccall unsafe "get_rand_bytes"
  c_get_rand_bytes :: Ptr CUChar -> CSize -> IO CInt

cpuHasRdRand :: IO Bool
cpuHasRdRand = (/= 0) `fmap` c_cpu_has_rdrand
#endif
