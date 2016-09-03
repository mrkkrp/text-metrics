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

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Ratio
import Data.Text (Text)
import Data.Text.Metrics
import Test.Hspec
import qualified Data.Text as T

main :: IO ()
main = hspec spec

-- TODO add property tests for properties like “we get the same result if we
-- swap arguments”

spec :: Spec
spec = do
  describe "levenshtein" $ do
    testPair levenshtein "kitten"   "sitting"  3
    testPair levenshtein "sitting"  "kitten"   3
    testPair levenshtein "cake"     "drake"    2
    testPair levenshtein "drake"    "cake"     2
    testPair levenshtein "saturday" "sunday"   3
    testPair levenshtein "sunday"   "saturday" 3
    testPair levenshtein "red"      "wax"      3
    testPair levenshtein "wax"      "red"      3
    testPair levenshtein "lucky"    "lucky"    0
    testPair levenshtein ""         ""         0
  describe "levenshteinNorm" $ do
    testPair levenshteinNorm "kitten"   "sitting"  (4 % 7)
    testPair levenshteinNorm "sitting"  "kitten"   (4 % 7)
    testPair levenshteinNorm "cake"     "drake"    (3 % 5)
    testPair levenshteinNorm "drake"    "cake"     (3 % 5)
    testPair levenshteinNorm "saturday" "sunday"   (5 % 8)
    testPair levenshteinNorm "sunday"   "saturday" (5 % 8)
    testPair levenshteinNorm "red"      "wax"      (0 % 1)
    testPair levenshteinNorm "wax"      "red"      (0 % 1)
    testPair levenshteinNorm "lucky"    "lucky"    (1 % 1)
    testPair levenshteinNorm ""         ""         (1 % 1)
  describe "damerauLevenshtein" $ do
    testPair damerauLevenshtein "veryvery long" "very long"     4
    testPair damerauLevenshtein "very long"     "veryvery long" 4
    testPair damerauLevenshtein "thing"         "think"         1
    testPair damerauLevenshtein "think"         "thing"         1
    testPair damerauLevenshtein "nose"          "ones"          2
    testPair damerauLevenshtein "ones"          "nose"          2
    testPair damerauLevenshtein "thing"         "sign"          3
    testPair damerauLevenshtein "sign"          "thing"         3
    testPair damerauLevenshtein "red"           "wax"           3
    testPair damerauLevenshtein "wax"           "red"           3
    testPair damerauLevenshtein "lucky"         "lucky"         0
    testPair damerauLevenshtein ""              ""              0
  describe "damerauLevenshteinNorm" $ do
    testPair damerauLevenshteinNorm "veryvery long" "very long"     (9 % 13)
    testPair damerauLevenshteinNorm "very long"     "veryvery long" (9 % 13)
    testPair damerauLevenshteinNorm "thing"         "think"         (4 % 5)
    testPair damerauLevenshteinNorm "think"         "thing"         (4 % 5)
    testPair damerauLevenshteinNorm "nose"          "ones"          (1 % 2)
    testPair damerauLevenshteinNorm "ones"          "nose"          (1 % 2)
    testPair damerauLevenshteinNorm "thing"         "sign"          (2 % 5)
    testPair damerauLevenshteinNorm "sign"          "thing"         (2 % 5)
    testPair damerauLevenshteinNorm "red"           "wax"           (0 % 1)
    testPair damerauLevenshteinNorm "wax"           "red"           (0 % 1)
    testPair damerauLevenshteinNorm "lucky"         "lucky"         (1 % 1)
    testPair damerauLevenshteinNorm ""              ""              (1 % 1)
  describe "hamming" $ do
    testPair hamming "karolin" "kathrin" (Just 3)
    testPair hamming "kathrin" "karolin" (Just 3)
    testPair hamming "karolin" "kerstin" (Just 3)
    testPair hamming "kerstin" "karolin" (Just 3)
    testPair hamming "1011101" "1001001" (Just 2)
    testPair hamming "1001001" "1011101" (Just 2)
    testPair hamming "2173896" "2233796" (Just 3)
    testPair hamming "2233796" "2173896" (Just 3)
    testPair hamming "toned"   "roses"   (Just 3)
    testPair hamming "roses"   "toned"   (Just 3)
    testPair hamming "red"     "wax"     (Just 3)
    testPair hamming "wax"     "red"     (Just 3)
    testPair hamming "lucky"   "lucky"   (Just 0)
    testPair hamming ""        ""        (Just 0)
    testPair hamming "small"   "big"     Nothing

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
