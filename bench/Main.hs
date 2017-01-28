--
-- Benchmarks for the ‘text-metrics’ package.
--
-- Copyright © 2016–2017 Mark Karpov <markkarpov@openmailbox.org>
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

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Text (Text)
import Data.Text.Metrics
import qualified Data.Text as T

main :: IO ()
main = defaultMain
  [ btmetric "levenshtein"            levenshtein
  , btmetric "levenshteinNorm"        levenshteinNorm
  , btmetric "damerauLevenshtein"     damerauLevenshtein
  , btmetric "damerauLevenshteinNorm" damerauLevenshteinNorm
  , btmetric "hamming"                hamming
  , btmetric "jaro"                   jaro
  , btmetric "jaroWinkler"            jaroWinkler ]

-- | Produce benchmark group to test.

btmetric :: NFData a => String -> (Text -> Text -> a) -> Benchmark
btmetric name f = bgroup name (bs <$> stdSeries)
  where
    bs n = env (return (testData n, testData n)) (bench (show n) . nf (uncurry f))

-- | The series of lengths to try with every function as part of 'btmetric'.

stdSeries :: [Int]
stdSeries = [5,10,20,40,80,160]

testData :: Int -> Text
testData n = T.pack . take n . drop (n `mod` 4) . cycle $ ['a'..'z']
