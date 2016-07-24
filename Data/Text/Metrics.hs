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
levenshtein _ _ = 0 -- TODO

-- | Normalized Levenshtein distance between two 'Text' values.

levenshteinNorm :: Text -> Text -> Ratio Natural
levenshteinNorm _ _ = 1 % 1 -- TODO

-- | Damerau-Levenshtein distance between two 'Text' values.

damerauLevenshtein :: Text -> Text -> Natural
damerauLevenshtein _ _ = 0 -- TODO

-- | Normalized damerau-Levenshtein distance between two 'Text' values.

damerauLevenshteinNorm :: Text -> Text -> Ratio Natural
damerauLevenshteinNorm _ _ = 1 % 1 -- TODO

----------------------------------------------------------------------------
-- Other

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
