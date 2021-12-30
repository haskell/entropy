{-|
 Maintainer: Thomas.DuBuisson@gmail.com
 Stability: beta
 Portability: portable

 Obtain entropy from system sources or x86 RDRAND when available.

-}

module System.EntropyGhcjs
        ( CryptHandle
        , openHandle
        , hGetEntropy
        , closeHandle
        , hardwareRandom
        ) where

import Data.ByteString as B
import GHCJS.DOM.Crypto as Crypto
import GHCJS.DOM.Types (ArrayBufferView (..), fromJSValUnchecked)
import GHCJS.DOM.GlobalCrypto (getCrypto)
import GHCJS.DOM (globalThisUnchecked)
import Language.Javascript.JSaddle.Object as JS


-- |Handle for manual resource management
newtype CryptHandle = CH Crypto

-- | Get random values from the hardware RNG or return Nothing if no
-- supported hardware RNG is available.
--
-- Not supported on ghcjs.
hardwareRandom :: Int -> IO (Maybe B.ByteString)
hardwareRandom _ = pure Nothing

-- |Open a `CryptHandle`
openHandle :: IO CryptHandle
openHandle = do
  this <- globalThisUnchecked
  CH <$> getCrypto this

-- |Close the `CryptHandle`
closeHandle :: CryptHandle -> IO ()
closeHandle _ = pure ()

-- |Read random data from a `CryptHandle`
hGetEntropy :: CryptHandle -> Int -> IO B.ByteString
hGetEntropy (CH h) n = do
  arr <- JS.new (jsg "Int8Array") [n]
  getRandomValues_ h (ArrayBufferView arr)
  B.pack <$> fromJSValUnchecked arr
