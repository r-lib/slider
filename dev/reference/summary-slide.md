# Specialized sliding functions

These functions are specialized variants of the most common ways that
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) is
generally used. Notably, `slide_sum()` can be used for rolling sums, and
`slide_mean()` can be used for rolling averages.

These specialized variants are *much* faster and more memory efficient
than using an otherwise equivalent call constructed with
[`slide_dbl()`](https://slider.r-lib.org/dev/reference/slide.md) or
[`slide_lgl()`](https://slider.r-lib.org/dev/reference/slide.md),
especially with a very wide window.

## Usage

``` r
slide_sum(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
)

slide_prod(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
)

slide_mean(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
)

slide_min(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
)

slide_max(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
)

slide_all(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
)

slide_any(
  x,
  ...,
  before = 0L,
  after = 0L,
  step = 1L,
  complete = FALSE,
  na_rm = FALSE
)
```

## Arguments

- x:

  `[vector]`

  A vector to compute the sliding function on.

  - For sliding sum, mean, prod, min, and max, `x` will be cast to a
    double vector with
    [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).

  - For sliding any and all, `x` will be cast to a logical vector with
    [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).

- ...:

  These dots are for future extensions and must be empty.

- before, after:

  `[integer(1) / Inf]`

  The number of values before or after the current element to include in
  the sliding window. Set to `Inf` to select all elements before or
  after the current element. Negative values are allowed, which allows
  you to "look forward" from the current element if used as the
  `.before` value, or "look backwards" if used as `.after`.

- step:

  `[positive integer(1)]`

  The number of elements to shift the window forward between function
  calls.

- complete:

  `[logical(1)]`

  Should the function be evaluated on complete windows only? If `FALSE`,
  the default, then partial computations will be allowed.

- na_rm:

  `[logical(1)]`

  Should missing values be removed from the computation?

## Value

A vector the same size as `x` containing the result of applying the
summary function over the sliding windows.

- For sliding sum, mean, prod, min, and max, a double vector will be
  returned.

- For sliding any and all, a logical vector will be returned.

## Details

Note that these functions are *not* generic and do not respect method
dispatch of the corresponding summary function (i.e.
[`base::sum()`](https://rdrr.io/r/base/sum.html),
[`base::mean()`](https://rdrr.io/r/base/mean.html)). Input will always
be cast to a double or logical vector using
[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html),
and an internal method for computing the summary function will be used.

Due to the structure of segment trees, `slide_mean()` does not perform
the same "two pass" mean that
[`mean()`](https://rdrr.io/r/base/mean.html) does (the intention of the
second pass is to perform a floating point error correction). Because of
this, there may be small differences between `slide_mean(x)` and
`slide_dbl(x, mean)` in some cases.

## Implementation

These variants are implemented using a data structure known as a
*segment tree*, which allows for extremely fast repeated range queries
without loss of precision.

One alternative to segment trees is to directly recompute the summary
function on each full window. This is what is done by using, for
example, `slide_dbl(x, sum)`. This is extremely slow with large window
sizes and wastes a lot of effort recomputing nearly the same information
on each window. It can be made slightly faster by moving the sum to C to
avoid intermediate allocations, but it still fairly slow.

A second alternative is to use an *online* algorithm, which uses
information from the previous window to compute the next window. These
are extremely fast, only requiring a single pass through the data, but
often suffer from numerical instability issues.

Segment trees are an attempt to reconcile the performance issues of the
direct approach with the numerical issues of the online approach. The
performance of segment trees isn't quite as fast as online algorithms,
but is close enough that it should be usable on most large data sets
without any issues. Unlike online algorithms, segment trees don't suffer
from any extra numerical instability issues.

## References

Leis, Kundhikanjana, Kemper, and Neumann (2015). "Efficient Processing
of Window Functions in Analytical SQL Queries".
https://dl.acm.org/doi/10.14778/2794367.2794375

## See also

[`slide_index_sum()`](https://slider.r-lib.org/dev/reference/summary-index.md)

## Examples

``` r
x <- c(1, 5, 3, 2, 6, 10)

# `slide_sum()` can be used for rolling sums.
# The following are equivalent, but `slide_sum()` is much faster.
slide_sum(x, before = 2)
#> [1]  1  6  9 10 11 18
slide_dbl(x, sum, .before = 2)
#> [1]  1  6  9 10 11 18

# `slide_mean()` can be used for rolling averages
slide_mean(x, before = 2)
#> [1] 1.000000 3.000000 3.000000 3.333333 3.666667 6.000000

# Only evaluate the sum on complete windows
slide_sum(x, before = 2, after = 1, complete = TRUE)
#> [1] NA NA 11 16 21 NA

# Skip every other calculation
slide_sum(x, before = 2, step = 2)
#> [1]  1 NA  9 NA 11 NA
```
