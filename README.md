# Text Metrics

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/text-metrics.svg?style=flat)](https://hackage.haskell.org/package/text-metrics)
[![Stackage Nightly](http://stackage.org/package/text-metrics/badge/nightly)](http://stackage.org/nightly/package/text-metrics)
[![Stackage LTS](http://stackage.org/package/text-metrics/badge/lts)](http://stackage.org/lts/package/text-metrics)
![CI](https://github.com/mrkkrp/text-metrics/workflows/CI/badge.svg?branch=master)

The library provides efficient implementations of various strings metric
algorithms. It works with strict `Text` values.

The current version of the package implements:

* [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance)
* [Normalized Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance)
* [Damerau-Levenshtein distance](https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance)
* [Normalized Damerau-Levenshtein distance](https://en.wikipedia.org/wiki/Damerau%E2%80%93Levenshtein_distance)
* [Hamming distance](https://en.wikipedia.org/wiki/Hamming_distance)
* [Jaro distance](https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance)
* [Jaro-Winkler distance](https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance)
* [Overlap coefficient](https://en.wikipedia.org/wiki/Overlap_coefficient)
* [Jaccard similarity coefficient](https://en.wikipedia.org/wiki/Jaccard_index)

## Comparison with the `edit-distance` package

There is
[`edit-distance`](https://hackage.haskell.org/package/edit-distance) package
whose scope overlaps with the scope of this package. The differences are:

* `edit-distance` allows to specify costs for every operation when
  calculating Levenshtein distance (insertion, deletion, substitution, and
  transposition). This is rarely needed though in real-world applications,
  IMO.

* `edit-distance` only provides Levenshtein distance, `text-metrics` aims to
  provide implementations of most string metrics algorithms.

* `edit-distance` works on `Strings`, while `text-metrics` works on strict
  `Text` values.

## Implementation

Although we originally used C for speed, currently all functions are pure
Haskell tuned for performance. See [this blog
post](https://markkarpov.com/post/migrating-text-metrics.html) for more
info.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/text-metrics/issues).

Pull requests are also welcome.

## License

Copyright © 2016–present Mark Karpov

Distributed under BSD 3 clause license.
