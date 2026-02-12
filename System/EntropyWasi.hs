{-# LANGUAGE CApiFFI #-}

module System.EntropyWasi where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Internal as B
import Data.Word (Word8)
import Foreign.C.Types
import Foreign.Ptr

data CryptHandle = BogusCryptHandle

openHandle :: IO CryptHandle
openHandle = pure BogusCryptHandle

hGetEntropy :: CryptHandle -> Int -> IO ByteString
hGetEntropy BogusCryptHandle = \n -> B.create n $ go (fromIntegral n)
  where
    go :: CSize -> Ptr Word8 -> IO ()
    go n ptr
      | n <= 256 = getentropy' ptr n
      | otherwise = do
          getentropy' ptr 256
          go (n - 256) (ptr `plusPtr` 256)

    getentropy' ptr n = do
      res <- getentropy ptr n
      when (res /= 0) $
        fail "getentropy failed"

foreign import capi safe "unistd.h getentropy"
  getentropy :: Ptr Word8 -> CSize -> IO CInt

closeHandle :: CryptHandle -> IO ()
closeHandle BogusCryptHandle = pure ()

hardwareRandom :: Int -> IO (Maybe ByteString)
hardwareRandom _ = pure Nothing
