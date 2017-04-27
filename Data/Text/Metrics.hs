-- |
-- Module      :  Data.Text.Metrics
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides efficient implementations of various strings metric
-- algorithms. It works with strict 'Text' values and returns either
-- 'Natural' numbers (because the metrics cannot be negative), or @'Ratio'
-- 'Natural'@ values because returned values are rational non-negative
-- numbers by definition.
--
-- The functions provided here are the fastest implementations available for
-- use in Haskell programs. In fact the functions are implemented in C for
-- maximal efficiency, but this leads to a minor flaw. When we work with
-- 'Text' values in C, they are represented as UTF-16 encoded strings of
-- two-byte values. The algorithms treat the strings as if a character
-- corresponds to one element in such strings, which is true for almost all
-- modern text data. However, there are characters that are represented by
-- two adjoined elements in UTF-16: emoji, historic scripts, less used
-- Chinese ideographs, and some more. If input 'Text' of the functions
-- contains such characters, the functions may return slightly incorrect
-- result. Decide for yourself if this is acceptable for your use case, but
-- chances are you will never run into situations when the functions produce
-- incorrect results.

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
  , hamming
  , jaro
  , jaroWinkler )
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
-- Levenshtein distance between two strings is the minimal number of
-- operations necessary to transform one string into another. For
-- Levenshtein distance allowed operations are: deletion, insertion, and
-- substitution.
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
-- distance is defined as the number of positions at which the corresponding
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

-- | Return Jaro distance between two 'Text' values. Returned value is in
-- the range from 0 (no similarity) to 1 (exact match).
--
-- While the algorithm is pretty clear for artificial examples (like those
-- from the linked Wikipedia article), for /arbitrary/ strings, it may be
-- hard to decide which of two strings should be considered as one having
-- “reference” order of characters (since order of matching characters in an
-- essential part of the definition of the algorithm). This makes us
-- consider the first string the “reference” string (with correct order of
-- characters). Thus generally,
--
-- > jaro a b ≠ jaro b a
--
-- This asymmetry can be found in all implementations of the algorithm on
-- the internet, AFAIK.
--
-- See also: <http://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance>
--
-- @since 0.2.0

jaro :: Text -> Text -> Ratio Natural
jaro = jaroCommon (\_ _ _ _ x -> return x)

jaroCommon :: (CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> Ratio Natural -> IO (Ratio Natural)) -> Text -> Text -> Ratio Natural
jaroCommon f a b = unsafePerformIO $ alloca $ \m' -> alloca $ \t' ->
  TF.useAsPtr a $ \aptr asize ->
    TF.useAsPtr b $ \bptr bsize ->
      if asize == 0 || bsize == 0
        then return (1 % 1)
        else do
          let asize' = fromIntegral asize
              bsize' = fromIntegral bsize
          c_jaro m' t' asize' aptr bsize' bptr
          m <- fromIntegral <$> peek m'
          t <- fromIntegral <$> peek t'
          f asize' aptr bsize' bptr $
            if m == 0
              then 0
              else ((m % fromIntegral asize) +
                    (m % fromIntegral bsize) +
                    ((m - t) % m)) / 3
{-# INLINE jaroCommon #-}

foreign import ccall unsafe "tmetrics_jaro"
  c_jaro :: Ptr CUInt -> Ptr CUInt -> CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> IO ()

-- | Return Jaro-Winkler distance between two 'Text' values. Returned value
-- is in range from 0 (no similarity) to 1 (exact match).
--
-- See also: <http://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance>
--
-- @since 0.2.0

jaroWinkler :: Text -> Text -> Ratio Natural
jaroWinkler = jaroCommon g
  where
    g asize aptr bsize bptr dj = do
      l <- fromIntegral <$> c_common_prefix asize aptr bsize bptr
      return (dj + (1  % 10) * l * (1 - dj))

foreign import ccall unsafe "tmetrics_common_prefix"
  c_common_prefix :: CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> IO CUInt

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
