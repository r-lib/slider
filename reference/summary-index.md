# Specialized sliding functions relative to an index

These functions are specialized variants of the most common ways that
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md) is
generally used. Notably, `slide_index_sum()` can be used for rolling
sums relative to an index (like a Date column), and `slide_index_mean()`
can be used for rolling averages.

These specialized variants are *much* faster and more memory efficient
than using an otherwise equivalent call constructed with
[`slide_index_dbl()`](https://slider.r-lib.org/reference/slide_index.md)
or
[`slide_index_lgl()`](https://slider.r-lib.org/reference/slide_index.md),
especially with a very wide window.

## Usage

``` r
slide_index_sum(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
)

slide_index_prod(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
)

slide_index_mean(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
)

slide_index_min(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
)

slide_index_max(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
)

slide_index_all(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
  complete = FALSE,
  na_rm = FALSE
)

slide_index_any(
  x,
  i,
  ...,
  before = 0L,
  after = 0L,
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

- i:

  `[vector]`

  The index vector that determines the window sizes. It is fairly common
  to supply a date vector as the index, but not required.

  There are 3 restrictions on the index:

  - The size of the index must match the size of `.x`, they will not be
    recycled to their common size.

  - The index must be an *increasing* vector, but duplicate values are
    allowed.

  - The index cannot have missing values.

- ...:

  These dots are for future extensions and must be empty.

- before, after:

  `[vector(1) / function / Inf]`

  - If a vector of size 1, these represent the number of values before
    or after the current element of `.i` to include in the sliding
    window. Negative values are allowed, which allows you to "look
    forward" from the current element if used as the `.before` value, or
    "look backwards" if used as `.after`. Boundaries are computed from
    these elements as `.i - .before` and `.i + .after`. Any object that
    can be added or subtracted from `.i` with `+` and `-` can be used.
    For example, a lubridate period, such as
    [`lubridate::weeks()`](https://lubridate.tidyverse.org/reference/period.html).

  - If `Inf`, this selects all elements before or after the current
    element.

  - If a function, or a one-sided formula which can be coerced to a
    function, it is applied to `.i` to compute the boundaries. Note that
    this function will only be applied to the *unique* values of `.i`,
    so it should not rely on the original length of `.i` in any way.
    This is useful for applying a complex arithmetic operation that
    can't be expressed with a single `-` or `+` operation. One example
    would be to use
    [`lubridate::add_with_rollback()`](https://lubridate.tidyverse.org/reference/mplus.html)
    to avoid invalid dates at the end of the month.

  The ranges that result from applying `.before` and `.after` have the
  same 3 restrictions as `.i` itself, and are cast to the type of `.i`
  using
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).

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

For more details about the implementation, see the help page of
[`slide_sum()`](https://slider.r-lib.org/reference/summary-slide.md).

## See also

[`slide_sum()`](https://slider.r-lib.org/reference/summary-slide.md)

## Examples

``` r
x <- c(1, 5, 3, 2, 6, 10)
i <- as.Date("2019-01-01") + c(0, 1, 3, 4, 6, 8)

# `slide_index_sum()` can be used for rolling sums relative to an index,
# allowing you to "respect gaps" in your series. Notice that the rolling
# sum in row 3 is only computed from `2019-01-04` and `2019-01-02` since
# `2019-01-01` is more than two days before the current date.
data.frame(
  i = i,
  x = x,
  roll = slide_index_sum(x, i, before = 2)
)
#>            i  x roll
#> 1 2019-01-01  1    1
#> 2 2019-01-02  5    6
#> 3 2019-01-04  3    8
#> 4 2019-01-05  2    5
#> 5 2019-01-07  6    8
#> 6 2019-01-09 10   16

# `slide_index_mean()` can be used for rolling averages
slide_index_mean(x, i, before = 2)
#> [1] 1.0 3.0 4.0 2.5 4.0 8.0

# Only evaluate the sum on windows that have the potential to be complete
slide_index_sum(x, i, before = 2, after = 1, complete = TRUE)
#> [1] NA NA 10  5  8 NA
```
