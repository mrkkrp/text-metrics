module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Metrics

main :: IO ()
main =
  defaultMain
    [ btmetric "levenshtein" levenshtein,
      btmetric "levenshteinNorm" levenshteinNorm,
      btmetric "damerauLevenshtein" damerauLevenshtein,
      btmetric "damerauLevenshteinNorm" damerauLevenshteinNorm,
      btmetric "overlap" overlap,
      btmetric "jaccard" jaccard,
      btmetric "hamming" hamming,
      btmetric "jaro" jaro,
      btmetric "jaroWinkler" jaroWinkler
    ]

-- | Produce benchmark group to test.
btmetric :: NFData a => String -> (Text -> Text -> a) -> Benchmark
btmetric name f = bgroup name (bs <$> stdSeries)
  where
    bs n = env (return (testData n, testData n)) (bench (show n) . nf (uncurry f))

-- | The series of lengths to try with every function as part of 'btmetric'.
stdSeries :: [Int]
stdSeries = [5, 10, 20, 40, 80, 160]

testData :: Int -> Text
testData n = T.pack . take n . drop (n `mod` 4) . cycle $ ['a' .. 'z']
