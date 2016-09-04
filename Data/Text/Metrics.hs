-- |
-- Module      :  Data.Text.Metrics
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides efficient implementations of various strings metrics.
-- It works with strict 'Text' values and returns either 'Natural' numbers
-- (because the metrics cannot be negative), or @'Ratio' 'Natural'@ values
-- because returned values are rational non-negative numbers by definition.

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}

module Data.Text.Metrics
  ( -- * Levenshtein variants
    levenshtein
  , levenshteinNorm
  , damerauLevenshtein
  , damerauLevenshteinNorm
    -- * Other
  , hamming )
where

import Data.Ratio
import Data.Text
import Foreign
import Foreign.C.Types
import Numeric.Natural
import System.IO.Unsafe
import qualified Data.Text           as T
import qualified Data.Text.Foreign   as TF

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

----------------------------------------------------------------------------
-- Levenshtein variants

-- | Return Levenshtein distance between two 'Text' values. Classic
-- Levenshtein distance between two strings is minimal number of operations
-- necessary to transform one string into another. For Levenshtein distance
-- allowed operations are: deletion, insertion, and substitution.
--
-- See also: <https://en.wikipedia.org/wiki/Levenshtein_distance>.

levenshtein :: Text -> Text -> Natural
levenshtein = withTwo c_levenshtein

foreign import ccall unsafe "tmetrics_levenshtein"
  c_levenshtein :: CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> IO CUInt

-- | Return normalized Levenshtein distance between two 'Text' values.
-- Result is a non-negative rational number (represented as @'Ratio'
-- 'Natural'@), where 0 signifies no similarity between the strings, while 1
-- means exact match. The operation is virtually as fast as 'levenshtein'.
--
-- See also: <https://en.wikipedia.org/wiki/Levenshtein_distance>.

levenshteinNorm :: Text -> Text -> Ratio Natural
levenshteinNorm = norm levenshtein
{-# INLINE levenshteinNorm #-}

-- | Return Damerau-Levenshtein distance between two 'Text' values. The
-- function works like 'levenshtein', but the collection of allowed
-- operations also includes transposition of two /adjacent/ characters. The
-- function is about 20% slower than 'levenshtein', but still pretty fast.
--
-- See also: <https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance>.

damerauLevenshtein :: Text -> Text -> Natural
damerauLevenshtein = withTwo c_damerau_levenshtein

foreign import ccall unsafe "tmetrics_damerau_levenshtein"
  c_damerau_levenshtein :: CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> IO CUInt

-- | Return normalized Damerau-Levenshtein distance between two 'Text'
-- values. Result is a non-negative rational number (represented as @'Ratio'
-- 'Natural'@), where 0 signifies no similarity between the strings, while 1
-- means exact match. The operation is virtually as fast as
-- 'damerauLevenshtein'.
--
-- See also: <https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance>.

damerauLevenshteinNorm :: Text -> Text -> Ratio Natural
damerauLevenshteinNorm = norm damerauLevenshtein
{-# INLINE damerauLevenshteinNorm #-}

----------------------------------------------------------------------------
-- Other

-- | /O(n)/ Return Hamming distance between two 'Text' values. Hamming
-- distance is defined as number of positions at which the corresponding
-- symbols are different. The input 'Text' values should be of equal length
-- or 'Nothing' will be returned.
--
-- See also: <https://en.wikipedia.org/wiki/Hamming_distance>.

hamming :: Text -> Text -> Maybe Natural
hamming a b =
  if T.length a == T.length b
    then Just . unsafePerformIO . TF.useAsPtr a $ \aptr size ->
      TF.useAsPtr b $ \bptr _ ->
        fromIntegral <$> c_hamming (fromIntegral size) aptr bptr
    else Nothing

foreign import ccall unsafe "tmetrics_hamming"
  c_hamming :: CUInt -> Ptr Word16 -> Ptr Word16 -> IO CUInt

----------------------------------------------------------------------------
-- Helpers

withTwo
  :: (CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> IO CUInt)
  -> Text
  -> Text
  -> Natural
withTwo f a b =
  unsafePerformIO . TF.useAsPtr a $ \aptr asize ->
    TF.useAsPtr b $ \bptr bsize ->
      fromIntegral <$> f (fromIntegral asize) aptr (fromIntegral bsize) bptr
{-# INLINE withTwo #-}

norm :: (Text -> Text -> Natural) -> Text -> Text -> Ratio Natural
norm f a b =
  let r = f a b
  in if r == 0
       then 1 % 1
       else 1 % 1 - r % fromIntegral (max (T.length a) (T.length b))
{-# INLINE norm #-}
