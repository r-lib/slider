# Break a vector into blocks

`block()` breaks up the `i`-ndex by `period`, and then uses that to
define the indices to chop `x` with.

For example, it can split `x` into monthly or yearly blocks. Combined
with [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html),
it is a way to iterate over a vector in "time blocks".

## Usage

``` r
block(x, i, period, every = 1L, origin = NULL)
```

## Arguments

- x:

  `[vector]`

  The vector to block.

- i:

  `[Date / POSIXct / POSIXlt]`

  The datetime index to block by.

  There are 3 restrictions on the index:

  - The size of the index must match the size of `x`, they will not be
    recycled to their common size.

  - The index must be an *increasing* vector, but duplicate values are
    allowed.

  - The index cannot have missing values.

- period:

  `[character(1)]`

  A string defining the period to group by. Valid inputs can be roughly
  broken into:

  - `"year"`, `"quarter"`, `"month"`, `"week"`, `"day"`

  - `"hour"`, `"minute"`, `"second"`, `"millisecond"`

  - `"yweek"`, `"mweek"`

  - `"yday"`, `"mday"`

- every:

  `[positive integer(1)]`

  The number of periods to group together.

  For example, if the period was set to `"year"` with an every value of
  `2`, then the years 1970 and 1971 would be placed in the same group.

- origin:

  `[Date(1) / POSIXct(1) / POSIXlt(1) / NULL]`

  The reference date time value. The default when left as `NULL` is the
  epoch time of `1970-01-01 00:00:00`, *in the time zone of the index*.

  This is generally used to define the anchor time to count from, which
  is relevant when the every value is `> 1`.

## Value

A vector fulfilling the following invariants:

- `vec_size(block(x)) == vec_size(unique(warp::warp_boundary(i)))`

- `vec_ptype(block(x)) == list()`

- `vec_ptype(block(x)[[1]]) == vec_ptype(x)`

## Details

`block()` determines the indices to block by with
[`warp::warp_boundary()`](https://davisvaughan.github.io/warp/reference/warp_boundary.html),
and splits `x` by those indices using
[`vctrs::vec_chop()`](https://vctrs.r-lib.org/reference/vec_chop.html).

Like [`slide()`](https://slider.r-lib.org/dev/reference/slide.md),
`block()` splits data frame `x` values row wise.

## See also

[`slide_period()`](https://slider.r-lib.org/dev/reference/slide_period.md),
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md),
[`slide_index()`](https://slider.r-lib.org/dev/reference/slide_index.md)

## Examples

``` r
x <- 1:6
i <- as.Date("2019-01-01") + c(-2:2, 31)

block(i, i, period = "year")
#> [[1]]
#> [1] "2018-12-30" "2018-12-31"
#> 
#> [[2]]
#> [1] "2019-01-01" "2019-01-02" "2019-01-03" "2019-02-01"
#> 

# Data frames are split row wise
df <- data.frame(x = x, i = i)
block(df, i, period = "month")
#> [[1]]
#>   x          i
#> 1 1 2018-12-30
#> 2 2 2018-12-31
#> 
#> [[2]]
#>   x          i
#> 1 3 2019-01-01
#> 2 4 2019-01-02
#> 3 5 2019-01-03
#> 
#> [[3]]
#>   x          i
#> 1 6 2019-02-01
#> 

# Iterate over these blocks to apply a function over
# non-overlapping period blocks. For example, to compute a
# mean over yearly or monthly blocks.
vapply(block(x, i, "year"), mean, numeric(1))
#> [1] 1.5 4.5
vapply(block(x, i, "month"), mean, numeric(1))
#> [1] 1.5 4.0 6.0

# block by every 2 months, ensuring that we start counting
# the 1st of the 2 months from `2019-01-01`
block(i, i, period = "month", every = 2, origin = as.Date("2019-01-01"))
#> [[1]]
#> [1] "2018-12-30" "2018-12-31"
#> 
#> [[2]]
#> [1] "2019-01-01" "2019-01-02" "2019-01-03" "2019-02-01"
#> 

# Use the `origin` to instead start counting from `2018-12-01`, meaning
# that [2018-12, 2019-01] gets bucketed together.
block(i, i, period = "month", every = 2, origin = as.Date("2018-12-01"))
#> [[1]]
#> [1] "2018-12-30" "2018-12-31" "2019-01-01" "2019-01-02" "2019-01-03"
#> 
#> [[2]]
#> [1] "2019-02-01"
#> 
```
