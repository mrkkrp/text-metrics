## Text Metrics 0.3.2

* Works with `text-2.0`.

## Text Metrics 0.3.1

* Fixed a bug in the implementation of Jaro-Winkler distance when two
  strings share a long prefix. [PR
  21](https://github.com/mrkkrp/text-metrics/pull/21).

* Dropped support for GHC 8.6 and older.

## Text Metrics 0.3.0

* All functions are now implemented in pure Haskell.

* All functions return `Int` or `Ratio Int` instead of `Natural` and `Ratio
  Natural`.

* Added `overlap` (returns overlap coefficient) and `jaccard` (returns
  Jaccard similarity coefficient).

## Text Metrics 0.2.0

* Made the `levenshtein`, `levenshteinNorm`, `damerauLevenshtein`, and
  `demerauLevenshtein` more efficient.

* Added `jaro` and `jaroWinkler` functions.

## Text Metrics 0.1.0

* Initial release.
