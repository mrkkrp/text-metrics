{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Ratio
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Metrics
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "levenshtein" $ do
    testSwap levenshtein
    context "with concrete examples" $ do
      testPair levenshtein "kitten" "sitting" 3
      testPair levenshtein "cake" "drake" 2
      testPair levenshtein "saturday" "sunday" 3
      testPair levenshtein "red" "wax" 3
      testPair levenshtein "a😀c" "abc" 1
      testPair levenshtein "lucky" "lucky" 0
      testPair levenshtein "" "" 0
  describe "levenshteinNorm" $ do
    testSwap levenshteinNorm
    testPair levenshteinNorm "kitten" "sitting" (4 % 7)
    testPair levenshteinNorm "cake" "drake" (3 % 5)
    testPair levenshteinNorm "saturday" "sunday" (5 % 8)
    testPair levenshteinNorm "red" "wax" (0 % 1)
    testPair levenshteinNorm "a😀c" "abc" (2 % 3)
    testPair levenshteinNorm "lucky" "lucky" (1 % 1)
    testPair levenshteinNorm "" "" (1 % 1)
  describe "damerauLevenshtein" $ do
    testSwap damerauLevenshtein
    testPair damerauLevenshtein "veryvery long" "very long" 4
    testPair damerauLevenshtein "thing" "think" 1
    testPair damerauLevenshtein "nose" "ones" 2
    testPair damerauLevenshtein "thing" "sign" 3
    testPair damerauLevenshtein "red" "wax" 3
    testPair damerauLevenshtein "a😀c" "abc" 1
    testPair damerauLevenshtein "lucky" "lucky" 0
    testPair damerauLevenshtein "" "" 0
  describe "damerauLevenshteinNorm" $ do
    testSwap damerauLevenshteinNorm
    testPair damerauLevenshteinNorm "veryvery long" "very long" (9 % 13)
    testPair damerauLevenshteinNorm "thing" "think" (4 % 5)
    testPair damerauLevenshteinNorm "nose" "ones" (1 % 2)
    testPair damerauLevenshteinNorm "thing" "sign" (2 % 5)
    testPair damerauLevenshteinNorm "red" "wax" (0 % 1)
    testPair damerauLevenshteinNorm "a😀c" "abc" (2 % 3)
    testPair damerauLevenshteinNorm "lucky" "lucky" (1 % 1)
    testPair damerauLevenshteinNorm "" "" (1 % 1)
  describe "hamming" $ do
    testSwap hamming
    testPair hamming "karolin" "kathrin" (Just 3)
    testPair hamming "karolin" "kerstin" (Just 3)
    testPair hamming "1011101" "1001001" (Just 2)
    testPair hamming "2173896" "2233796" (Just 3)
    testPair hamming "toned" "roses" (Just 3)
    testPair hamming "red" "wax" (Just 3)
    testPair hamming "a😀c" "abc" (Just 1)
    testPair hamming "lucky" "lucky" (Just 0)
    testPair hamming "" "" (Just 0)
    testPair hamming "small" "big" Nothing
  describe "jaro" $ do
    testPair jaro "aa" "a" (5 % 6)
    testPair jaro "a" "aa" (5 % 6)
    testPair jaro "martha" "marhta" (17 % 18)
    testPair jaro "marhta" "martha" (17 % 18)
    testPair jaro "dwayne" "duane" (37 % 45)
    testPair jaro "duane" "dwayne" (37 % 45)
    testPair jaro "dixon" "dicksonx" (23 % 30)
    testPair jaro "dicksonx" "dixon" (23 % 30)
    testPair jaro "jones" "johnson" (83 % 105)
    testPair jaro "johnson" "jones" (83 % 105)
    testPair jaro "brain" "brian" (14 % 15)
    testPair jaro "brian" "brain" (14 % 15)
    testPair jaro "five" "ten" (0 % 1)
    testPair jaro "ten" "five" (0 % 1)
    testPair jaro "lucky" "lucky" (1 % 1)
    testPair jaro "a😀c" "abc" (7 % 9)
    testPair jaro "" "" (0 % 1)
  describe "jaroWinkler" $ do
    testPair jaroWinkler "aa" "a" (17 % 20)
    testPair jaroWinkler "a" "aa" (17 % 20)
    testPair jaroWinkler "martha" "marhta" (173 % 180)
    testPair jaroWinkler "marhta" "martha" (173 % 180)
    testPair jaroWinkler "dwayne" "duane" (21 % 25)
    testPair jaroWinkler "duane" "dwayne" (21 % 25)
    testPair jaroWinkler "dixon" "dicksonx" (61 % 75)
    testPair jaroWinkler "dicksonx" "dixon" (61 % 75)
    testPair jaroWinkler "jones" "johnson" (437 % 525)
    testPair jaroWinkler "johnson" "jones" (437 % 525)
    testPair jaroWinkler "brain" "brian" (71 % 75)
    testPair jaroWinkler "brian" "brain" (71 % 75)
    testPair jaroWinkler "five" "ten" (0 % 1)
    testPair jaroWinkler "ten" "five" (0 % 1)
    testPair jaroWinkler "lucky" "lucky" (1 % 1)
    testPair jaroWinkler "a😀c" "abc" (4 % 5)
    testPair jaroWinkler "" "" (0 % 1)
    testPair jaroWinkler "aaaaaaaaaab" "aaaaaaaaaa" (54 % 55)
    testPair jaroWinkler "aaaaaaaaaaaaaaaaaaaab" "aaaaaaaaaaaaaaaaaaaa" (104 % 105)
  describe "overlap" $ do
    testSwap overlap
    testPair overlap "fly" "butterfly" (1 % 1)
    testPair overlap "night" "nacht" (3 % 5)
    testPair overlap "context" "contact" (5 % 7)
    testPair overlap "red" "wax" (0 % 1)
    testPair overlap "a😀c" "abc" (2 % 3)
    testPair overlap "lucky" "lucky" (1 % 1)
  describe "jaccard" $ do
    testSwap jaccard
    testPair jaccard "xxx" "xyx" (1 % 2)
    testPair jaccard "night" "nacht" (3 % 7)
    testPair jaccard "context" "contact" (5 % 9)
    testPair overlap "a😀c" "abc" (2 % 3)
    testPair jaccard "lucky" "lucky" (1 % 1)

-- | Test that given function returns the same results when order of
-- arguments is swapped.
testSwap :: (Eq a, Show a) => (Text -> Text -> a) -> SpecWith ()
testSwap f = context "if we swap the arguments" $
  it "produces the same result" $
    property $ \a b ->
      f a b === f b a

-- | Create spec for given metric function applying it to two 'Text' values
-- and comparing the result with expected one.
testPair ::
  (Eq a, Show a) =>
  -- | Function to test
  (Text -> Text -> a) ->
  -- | First input
  Text ->
  -- | Second input
  Text ->
  -- | Expected result
  a ->
  SpecWith ()
testPair f a b r =
  it ("‘" ++ T.unpack a ++ "’ and ‘" ++ T.unpack b ++ "’") $
    f a b `shouldBe` r
