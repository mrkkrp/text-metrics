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

import Data.Text (Text)
import Data.Text.Metrics
import Test.Hspec
import qualified Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec =
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

testPair :: (Eq a, Show a)
  => (Text -> Text -> a) -- ^ Function to test
  -> Text              -- ^ First input
  -> Text              -- ^ Second input
  -> a                 -- ^ Expected result
  -> SpecWith ()
testPair f a b r = it ("‘" ++ T.unpack a ++ "’ and ‘" ++ T.unpack b ++ "’") $
  f a b `shouldBe` r
