-- |
-- Module      :  Data.Text.Metrics
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides efficient implementations of various strings metric
-- algorithms. It works with strict 'Text' values.
--
-- __Note__: before version /0.3.0/ the package used C implementations of
-- the algorithms under the hood. Beginning from version /0.3.0/, the
-- implementations are written in Haskell while staying almost as fast, see:
--
-- <https://markkarpov.com/post/migrating-text-metrics.html>

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP          #-}
{-# LANGUAGE MultiWayIf   #-}

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

import Control.Monad
import Control.Monad.ST
import Data.Ratio
import Data.Text
import Data.Word (Word8)
import GHC.Exts (inline)
import qualified Data.Text                   as T
import qualified Data.Text.Unsafe            as TU
import qualified Data.Vector.Unboxed.Mutable as VUM

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
--
-- __Heads up__, before version /0.3.0/ this function returned
-- 'Data.Numeric.Natural'.

levenshtein :: Text -> Text -> Int
levenshtein a b = fst (levenshtein_ a b)

-- | Return normalized Levenshtein distance between two 'Text' values.
-- Result is a non-negative rational number (represented as @'Ratio'
-- 'Data.Numeric.Natural'@), where 0 signifies no similarity between the
-- strings, while 1 means exact match.
--
-- See also: <https://en.wikipedia.org/wiki/Levenshtein_distance>.
--
-- __Heads up__, before version /0.3.0/ this function returned @'Ratio'
-- 'Data.Numeric.Natural'@.

levenshteinNorm :: Text -> Text -> Ratio Int
levenshteinNorm = norm levenshtein_

-- | An internal helper, returns Levenshtein distance as the first element
-- of the tuple and max length of the two inputs as the second element of
-- the tuple.

levenshtein_ :: Text -> Text -> (Int, Int)
levenshtein_ a b
  | T.null a = (lenb, lenm)
  | T.null b = (lena, lenm)
  | otherwise = runST $ do
      let v_len = lenb + 1
      v <- VUM.unsafeNew (v_len * 2)
      let gov !i =
            when (i < v_len) $ do
              VUM.unsafeWrite v i i
              gov (i + 1)
          goi !i !na !v0 !v1 = do
            let !(TU.Iter ai da) = TU.iter a na
                goj !j !nb =
                  when (j < lenb) $ do
                    let !(TU.Iter bj db) = TU.iter b nb
                        cost = if ai == bj then 0 else 1
                    x <- (+ 1) <$> VUM.unsafeRead v (v1 + j)
                    y <- (+ 1) <$> VUM.unsafeRead v (v0 + j + 1)
                    z <- (+ cost) <$> VUM.unsafeRead v (v0 + j)
                    VUM.unsafeWrite v (v1 + j + 1) (min x (min y z))
                    goj (j + 1) (nb + db)
            when (i < lena) $ do
              VUM.unsafeWrite v v1 (i + 1)
              goj 0 0
              goi (i + 1) (na + da) v1 v0
      gov 0
      goi 0 0 0 v_len
      ld <- VUM.unsafeRead v (lenb + if even lena then 0 else v_len)
      return (ld, lenm)
  where
    lena = T.length a
    lenb = T.length b
    lenm = max lena lenb
{-# INLINE levenshtein_ #-}

-- | Return Damerau-Levenshtein distance between two 'Text' values. The
-- function works like 'levenshtein', but the collection of allowed
-- operations also includes transposition of two /adjacent/ characters.
--
-- See also: <https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance>.
--
-- __Heads up__, before version /0.3.0/ this function returned
-- 'Data.Numeric.Natural'.

damerauLevenshtein :: Text -> Text -> Int
damerauLevenshtein a b = fst (damerauLevenshtein_ a b)

-- | Return normalized Damerau-Levenshtein distance between two 'Text'
-- values. 0 signifies no similarity between the strings, while 1 means
-- exact match.
--
-- See also: <https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance>.
--
-- __Heads up__, before version /0.3.0/ this function returned @'Ratio'
-- 'Data.Numeric.Natural'@.

damerauLevenshteinNorm :: Text -> Text -> Ratio Int
damerauLevenshteinNorm = norm damerauLevenshtein_

-- | An internal helper, returns Damerau-Levenshtein distance as the first
-- element of the tuple and max length of the two inputs as the second
-- element of the tuple.

damerauLevenshtein_ :: Text -> Text -> (Int, Int)
damerauLevenshtein_ a b
  | T.null a = (lenb, lenm)
  | T.null b = (lena, lenm)
  | otherwise = runST $ do
      let v_len = lenb + 1
      v <- VUM.unsafeNew (v_len * 3)
      let gov !i =
            when (i < v_len) $ do
              VUM.unsafeWrite v i i
              gov (i + 1)
          goi !i !na !v0 !v1 !v2 = do
            let !(TU.Iter ai da) = TU.iter a na
                goj !j !nb =
                  when (j < lenb) $ do
                    let !(TU.Iter bj db) = TU.iter b nb
                        cost = if ai == bj then 0 else 1
                    x <- (+ 1) <$> VUM.unsafeRead v (v1 + j)
                    y <- (+ 1) <$> VUM.unsafeRead v (v0 + j + 1)
                    z <- (+ cost) <$> VUM.unsafeRead v (v0 + j)
                    let g = min x (min y z)
                    VUM.unsafeWrite v (v1 + j + 1) g
                    val <- (+ cost) <$> VUM.unsafeRead v (v2 + j - 1)
                    let !(TU.Iter ai_1 _) = TU.iter a (na - da) -- ???
                        !(TU.Iter bj_1 _) = TU.iter b (nb - db)
                    when (i > 0      &&
                          j > 0      &&
                          ai == bj_1 &&
                          ai_1 == bj &&
                          val < g) $
                      VUM.unsafeWrite v (v1 + j + 1) val
                    goj (j + 1) (nb + db)
            when (i < lena) $ do
              VUM.unsafeWrite v v1 (i + 1)
              goj 0 0
              goi (i + 1) (na + da) v1 v2 v0
      gov 0
      goi 0 0 0 v_len (v_len * 2)
      ld <- VUM.unsafeRead v (lenb + (lena `mod` 3) * v_len)
      return (ld, lenm)
  where
    lena = T.length a
    lenb = T.length b
    lenm = max lena lenb
{-# INLINE damerauLevenshtein_ #-}

----------------------------------------------------------------------------
-- Other

-- | /O(n)/ Return Hamming distance between two 'Text' values. Hamming
-- distance is defined as the number of positions at which the corresponding
-- symbols are different. The input 'Text' values should be of equal length
-- or 'Nothing' will be returned.
--
-- See also: <https://en.wikipedia.org/wiki/Hamming_distance>.
--
-- __Heads up__, before version /0.3.0/ this function returned @'Maybe'
-- 'Data.Numeric.Natural'@.

hamming :: Text -> Text -> Maybe Int
hamming a b =
  if T.length a == T.length b
    then Just (go 0 0 0)
    else Nothing
  where
    go !na !nb !r =
      let !(TU.Iter cha da) = TU.iter a na
          !(TU.Iter chb db) = TU.iter b nb
      in if | na  == len -> r
            | cha /= chb -> go (na + da) (nb + db) (r + 1)
            | otherwise  -> go (na + da) (nb + db) r
    len = TU.lengthWord16 a

-- | Return Jaro distance between two 'Text' values. Returned value is in
-- the range from 0 (no similarity) to 1 (exact match).
--
-- While the algorithm is pretty clear for artificial examples (like those
-- from the linked Wikipedia article), for /arbitrary/ strings, it may be
-- hard to decide which of two strings should be considered as one having
-- “reference” order of characters (order of matching characters in an
-- essential part of the definition of the algorithm). This makes us
-- consider the first string the “reference” string (with correct order of
-- characters). Thus generally,
--
-- > jaro a b ≠ jaro b a
--
-- This asymmetry can be found in all implementations of the algorithm on
-- the internet, AFAIK.
--
-- See also: <https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance>
--
-- @since 0.2.0
--
-- __Heads up__, before version /0.3.0/ this function returned @'Ratio'
-- 'Data.Numeric.Natural'@.

jaro :: Text -> Text -> Ratio Int
jaro a b =
  if T.null a || T.null b
    then 0 % 1
    else runST $ do
      let lena = T.length a
          lenb = T.length b
          d =
            if lena >= 2 && lenb >= 2
              then max lena lenb `quot` 2 - 1
              else 0
      v <- VUM.replicate lenb (0 :: Word8)
      r <- VUM.replicate 3 (0 :: Int) -- tj, m, t
      let goi !i !na !fromb = do
            let !(TU.Iter ai da) = TU.iter a na
                (from, fromb') =
                  if i >= d
                    then (i - d, fromb + TU.iter_ b fromb)
                    else (0, 0)
                to = min (i + d + 1) lenb
                goj !j !nb =
                  when (j < to) $ do
                    let !(TU.Iter bj db) = TU.iter b nb
                    used <- (== 1) <$> VUM.unsafeRead v j
                    if not used && ai == bj
                      then do
                        tj <- VUM.unsafeRead r 0
                        if j < tj
                          then VUM.unsafeModify r (+ 1) 2
                          else VUM.unsafeWrite  r 0 j
                        VUM.unsafeWrite v j 1
                        VUM.unsafeModify r (+ 1) 1
                      else goj (j + 1) (nb + db)
            when (i < lena) $ do
              goj from fromb
              goi (i + 1) (na + da) fromb'
      goi 0 0 0
      m <- VUM.unsafeRead r 1
      t <- VUM.unsafeRead r 2
      return $
        if m == 0
          then 0 % 1
          else ((m % lena) +
                (m % lenb) +
                ((m - t) % m)) / 3

-- | Return Jaro-Winkler distance between two 'Text' values. Returned value
-- is in range from 0 (no similarity) to 1 (exact match).
--
-- See also: <https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance>
--
-- @since 0.2.0
--
-- __Heads up__, before version /0.3.0/ this function returned @'Ratio'
-- 'Data.Numeric.Natural'@.

jaroWinkler :: Text -> Text -> Ratio Int
jaroWinkler a b = dj + (1 % 10) * l * (1 - dj)
  where
    dj = inline (jaro a b)
    l  = fromIntegral (commonPrefix a b)

-- | Return length of common prefix two 'Text' values have.

commonPrefix :: Text -> Text -> Int
commonPrefix a b = go 0 0 0
  where
    go !na !nb !r =
      let !(TU.Iter cha da) = TU.iter a na
          !(TU.Iter chb db) = TU.iter b nb
      in if | na == lena -> r
            | nb == lenb -> r
            | cha == chb -> go (na + da) (nb + db) (r + 1)
            | otherwise  -> r
    lena = TU.lengthWord16 a
    lenb = TU.lengthWord16 b
{-# INLINE commonPrefix #-}

----------------------------------------------------------------------------
-- Helpers

norm :: (Text -> Text -> (Int, Int)) -> Text -> Text -> Ratio Int
norm f a b =
  let (r, l) = f a b
  in if r == 0
       then 1 % 1
       else 1 % 1 - r % l
{-# INLINE norm #-}
