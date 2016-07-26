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
  ( -- * Levenshtein variants
    levenshtein
  , levenshteinNorm
  , damerauLevenshtein
  , damerauLevenshteinNorm
    -- * Other
  , hamming
  , overlap
  , jaccard
  , jaro
  , jaroWinkler )
where

import Data.Ratio
import Data.Text
import Foreign
import Foreign.C.Types
import Numeric.Natural
import System.IO.Unsafe
import qualified Data.Text         as T
import qualified Data.Text.Foreign as TF

----------------------------------------------------------------------------
-- Levenshtein variants

-- | Levenshtein distance between two 'Text' values.

levenshtein :: Text -> Text -> Natural
levenshtein = withTwo c_levenshtein

foreign import ccall unsafe "tmetrics_levenshtein"
  c_levenshtein :: CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> IO CUInt

-- | Return normalized Levenshtein distance between two 'Text' values.
-- Result is a non-negative real number (represented as @'Ratio'
-- 'Natural'@), where 0 signifies no similarity between the strings, while 1
-- means exact match.

levenshteinNorm :: Text -> Text -> Ratio Natural
levenshteinNorm = norm levenshtein
{-# INLINE levenshteinNorm #-}

-- | Damerau-Levenshtein distance between two 'Text' values.

damerauLevenshtein :: Text -> Text -> Natural
damerauLevenshtein = withTwo c_damerau_levenshtein

foreign import ccall unsafe "tmetrics_damerau_levenshtein"
  c_damerau_levenshtein :: CUInt -> Ptr Word16 -> CUInt -> Ptr Word16 -> IO CUInt

-- | Return normalized damerau-Levenshtein distance between two 'Text'
-- values. Result is a non-negative real number (represented as @'Ration'
-- 'Natural'@), where 0 signifies no similarity between the strings, while 1
-- means exact match.

damerauLevenshteinNorm :: Text -> Text -> Ratio Natural
damerauLevenshteinNorm = norm damerauLevenshtein
{-# INLINE damerauLevenshteinNorm #-}

----------------------------------------------------------------------------
-- Other

-- | Calculate Hamming distance between two 'Text' values which should have
-- equal length or 'Nothing' will be returned.

hamming :: Text -> Text -> Maybe Natural
hamming a b =
  if T.length a == T.length b
    then Just . unsafePerformIO . TF.useAsPtr a $ \aptr size ->
      TF.useAsPtr b $ \bptr _ ->
        fromIntegral <$> c_hamming (fromIntegral size) aptr bptr
    else Nothing

foreign import ccall unsafe "tmetrics_hamming"
  c_hamming :: CUInt -> Ptr Word16 -> Ptr Word16 -> IO CUInt

-- | Overlap coefficient between two 'Text' values which both should be not
-- empty or 'Nothing' will be returned.

overlap :: Text -> Text -> Maybe (Ratio Natural)
overlap _ _ = Just (1 % 1) -- TODO

-- | Jaccard similarity coefficient between two 'Text' values.

jaccard :: Text -> Text -> Ratio Natural
jaccard _ _ = 1 % 1 -- TODO

-- | Jaro distance between two 'Text' values.

jaro :: Text -> Text -> Ratio Natural
jaro _ _ = 1 % 1 -- TODO

-- | Jaro-Winkler distance between two 'Text' values.

jaroWinkler :: Text -> Text -> Ratio Natural
jaroWinkler _ _ = 1 % 1 -- TODO

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
