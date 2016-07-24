-- |
-- Module      :  Data.Text.Metrics
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Efficient implementations of various strings metrics.

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Text.Metrics
  ( hamming )
where

import Data.Text
import Foreign
import Foreign.C.Types
import Numeric.Natural
import System.IO.Unsafe
import qualified Data.Text         as T
import qualified Data.Text.Foreign as TF

-- | Calculate Hamming distance between two 'Text' values which should have
-- equal length or 'Nothing' will be returned.

hamming :: Text -> Text -> Maybe Natural
hamming a b =
  if T.length a == T.length b
    then Just . unsafePerformIO . TF.useAsPtr a $ \aptr size ->
      TF.useAsPtr b $ \bptr _ ->
        fromIntegral <$> c_hamming_distance (fromIntegral size) aptr bptr
    else Nothing

foreign import ccall unsafe "tmetrics_hamming_distance"
  c_hamming_distance :: CUInt -> Ptr Word16 -> Ptr Word16 -> IO CUInt
