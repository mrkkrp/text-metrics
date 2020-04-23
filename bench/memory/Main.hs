module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Metrics
import Weigh

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  bmetric "levenshtein" levenshtein
  bmetric "levenshteinNorm" levenshteinNorm
  bmetric "damerauLevenshtein" damerauLevenshtein
  bmetric "damerauLevenshteinNorm" damerauLevenshteinNorm
  bmetric "overlap" overlap
  bmetric "jaccard" jaccard
  bmetric "hamming" hamming
  bmetric "jaro" jaro
  bmetric "jaroWinkler" jaroWinkler

-- | Perform a series to measurements with the same metric function.
bmetric ::
  NFData a =>
  -- | Name of the benchmark group
  String ->
  -- | The function to benchmark
  (Text -> Text -> a) ->
  Weigh ()
bmetric name f = forM_ stdSeries $ \n ->
  func (name ++ "/" ++ show n) (uncurry f) (testData n, testData n)

-- | The series of lengths to try with every function as part of 'btmetric'.
stdSeries :: [Int]
stdSeries = [5, 10, 20, 40, 80, 160]

testData :: Int -> Text
testData n = T.pack . take n . drop (n `mod` 4) . cycle $ ['a' .. 'z']
