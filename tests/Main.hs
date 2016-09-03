--
-- Tests for the ‘text-metrics’ package.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Ratio
import Data.Text (Text)
import Data.Text.Metrics
import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "levenshtein" $ do
    testSwap levenshtein
    context "with concrete examples" $ do
      testPair levenshtein "kitten"   "sitting" 3
      testPair levenshtein "cake"     "drake"   2
      testPair levenshtein "saturday" "sunday"  3
      testPair levenshtein "red"      "wax"     3
      testPair levenshtein "lucky"    "lucky"   0
      testPair levenshtein ""         ""        0
  describe "levenshteinNorm" $ do
    testSwap levenshteinNorm
    testPair levenshteinNorm "kitten"   "sitting" (4 % 7)
    testPair levenshteinNorm "cake"     "drake"   (3 % 5)
    testPair levenshteinNorm "saturday" "sunday"  (5 % 8)
    testPair levenshteinNorm "red"      "wax"     (0 % 1)
    testPair levenshteinNorm "lucky"    "lucky"   (1 % 1)
    testPair levenshteinNorm ""         ""        (1 % 1)
  describe "damerauLevenshtein" $ do
    testSwap damerauLevenshtein
    testPair damerauLevenshtein "veryvery long" "very long" 4
    testPair damerauLevenshtein "thing"         "think"     1
    testPair damerauLevenshtein "nose"          "ones"      2
    testPair damerauLevenshtein "thing"         "sign"      3
    testPair damerauLevenshtein "red"           "wax"       3
    testPair damerauLevenshtein "lucky"         "lucky"     0
    testPair damerauLevenshtein ""              ""          0
  describe "damerauLevenshteinNorm" $ do
    testSwap damerauLevenshteinNorm
    testPair damerauLevenshteinNorm "veryvery long" "very long" (9 % 13)
    testPair damerauLevenshteinNorm "thing"         "think"     (4 % 5)
    testPair damerauLevenshteinNorm "nose"          "ones"      (1 % 2)
    testPair damerauLevenshteinNorm "thing"         "sign"      (2 % 5)
    testPair damerauLevenshteinNorm "red"           "wax"       (0 % 1)
    testPair damerauLevenshteinNorm "lucky"         "lucky"     (1 % 1)
    testPair damerauLevenshteinNorm ""              ""          (1 % 1)
  describe "hamming" $ do
    testSwap hamming
    testPair hamming "karolin" "kathrin" (Just 3)
    testPair hamming "karolin" "kerstin" (Just 3)
    testPair hamming "1011101" "1001001" (Just 2)
    testPair hamming "2173896" "2233796" (Just 3)
    testPair hamming "toned"   "roses"   (Just 3)
    testPair hamming "red"     "wax"     (Just 3)
    testPair hamming "lucky"   "lucky"   (Just 0)
    testPair hamming ""        ""        (Just 0)
    testPair hamming "small"   "big"     Nothing

-- | Test that given function returns the same results when order of
-- arguments is swapped.

testSwap :: (Eq a, Show a) => (Text -> Text -> a) -> SpecWith ()
testSwap f = context "if we swap the arguments" $
  it "produces the same result" $
    property $ \a b -> f a b === f b a

-- | Create spec for given metric function applying it to two 'Text' values
-- and comparing the result with expected one.

testPair :: (Eq a, Show a)
  => (Text -> Text -> a) -- ^ Function to test
  -> Text              -- ^ First input
  -> Text              -- ^ Second input
  -> a                 -- ^ Expected result
  -> SpecWith ()
testPair f a b r = it ("‘" ++ T.unpack a ++ "’ and ‘" ++ T.unpack b ++ "’") $
  f a b `shouldBe` r
