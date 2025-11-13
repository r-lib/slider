# Slide relative to an index chunked by period

`slide_period()` breaks up the `.i`-ndex by `.period`, and then uses
that to define the indices to slide over `.x` with.

It can be useful for, say, sliding over daily data in monthly chunks.

The underlying engine for breaking up `.i` is
[`warp::warp_distance()`](https://davisvaughan.github.io/warp/reference/warp_distance.html).
If you need more information about the `.period` types, that is the best
place to look.

## Usage

``` r
slide_period(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_period_vec(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .ptype = NULL
)

slide_period_dbl(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_period_int(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_period_lgl(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_period_chr(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_period_dfr(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

slide_period_dfc(
  .x,
  .i,
  .period,
  .f,
  ...,
  .every = 1L,
  .origin = NULL,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
)
```

## Arguments

- .x:

  `[vector]`

  The vector to iterate over and apply `.f` to.

- .i:

  `[Date / POSIXct / POSIXlt]`

  A datetime index to break into periods.

  There are 3 restrictions on the index:

  - The size of the index must match the size of `.x`, they will not be
    recycled to their common size.

  - The index must be an *increasing* vector, but duplicate values are
    allowed.

  - The index cannot have missing values.

- .period:

  `[character(1)]`

  A string defining the period to group by. Valid inputs can be roughly
  broken into:

  - `"year"`, `"quarter"`, `"month"`, `"week"`, `"day"`

  - `"hour"`, `"minute"`, `"second"`, `"millisecond"`

  - `"yweek"`, `"mweek"`

  - `"yday"`, `"mday"`

- .f:

  `[function / formula]`

  If a **function**, it is used as is.

  If a **formula**, e.g. `~ .x + 2`, it is converted to a function.
  There are three ways to refer to the arguments:

  - For a single argument function, use `.`

  - For a two argument function, use `.x` and `.y`

  - For more arguments, use `..1`, `..2`, `..3` etc

  This syntax allows you to create very compact anonymous functions.

- ...:

  Additional arguments passed on to the mapped function.

- .every:

  `[positive integer(1)]`

  The number of periods to group together.

  For example, if the period was set to `"year"` with an every value of
  `2`, then the years 1970 and 1971 would be placed in the same group.

- .origin:

  `[Date(1) / POSIXct(1) / POSIXlt(1) / NULL]`

  The reference date time value. The default when left as `NULL` is the
  epoch time of `1970-01-01 00:00:00`, *in the time zone of the index*.

  This is generally used to define the anchor time to count from, which
  is relevant when the every value is `> 1`.

- .before, .after:

  `[integer(1) / Inf]`

  The number of values before or after the current element to include in
  the sliding window. Set to `Inf` to select all elements before or
  after the current element. Negative values are allowed, which allows
  you to "look forward" from the current element if used as the
  `.before` value, or "look backwards" if used as `.after`.

- .complete:

  `[logical(1)]`

  Should the function be evaluated on complete windows only? If `FALSE`,
  the default, then partial computations will be allowed.

- .ptype:

  `[vector(0) / NULL]`

  A prototype corresponding to the type of the output.

  If `NULL`, the default, the output type is determined by computing the
  common type across the results of the calls to `.f`.

  If supplied, the result of each call to `.f` will be cast to that
  type, and the final output will have that type.

  If `getOption("vctrs.no_guessing")` is `TRUE`, the `.ptype` must be
  supplied. This is a way to make production code demand fixed types.

- .names_to:

  This controls what to do with input names supplied in `...`.

  - By default, input names are
    [zapped](https://rlang.r-lib.org/reference/zap.html).

  - If a string, specifies a column where the input names will be
    copied. These names are often useful to identify rows with their
    original input. If a column name is supplied and `...` is not named,
    an integer column is used instead.

  - If `NULL`, the input names are used as row names.

- .name_repair:

  One of `"unique"`, `"universal"`, `"check_unique"`, `"unique_quiet"`,
  or `"universal_quiet"`. See
  [`vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

  With [`vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.html),
  the repair function is applied to all inputs separately. This is
  because
  [`vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.html) needs
  to align their columns before binding the rows, and thus needs all
  inputs to have unique names. On the other hand,
  [`vec_cbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
  applies the repair function after all inputs have been concatenated
  together in a final data frame. Hence
  [`vec_cbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
  allows the more permissive minimal names repair.

- .size:

  If, `NULL`, the default, will determine the number of rows in
  [`vec_cbind()`](https://vctrs.r-lib.org/reference/vec_bind.html)
  output by using the tidyverse [recycling
  rules](https://vctrs.r-lib.org/reference/theory-faq-recycling.html).

  Alternatively, specify the desired number of rows, and any inputs of
  length 1 will be recycled appropriately.

## Value

A vector fulfilling the following invariants:

### `slide_period()`

- `vec_size(slide_period(.x)) == vec_size(unique(warp::warp_distance(.i)))`

- `vec_ptype(slide_period(.x)) == list()`

### `slide_period_vec()` and `slide_period_*()` variants

- `vec_size(slide_period_vec(.x)) == vec_size(unique(warp::warp_distance(.i)))`

- `vec_size(slide_period_vec(.x)[[1]]) == 1L`

- `vec_ptype(slide_period_vec(.x, .ptype = ptype)) == ptype`

## See also

[`block()`](https://slider.r-lib.org/dev/reference/block.md),
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md),
[`slide_index()`](https://slider.r-lib.org/dev/reference/slide_index.md)

## Examples

``` r
i <- as.Date("2019-01-28") + 0:5

# Split `i` into 2-day periods to apply `.f` to
slide_period(i, i, "day", identity, .every = 2)
#> [[1]]
#> [1] "2019-01-28" "2019-01-29"
#> 
#> [[2]]
#> [1] "2019-01-30" "2019-01-31"
#> 
#> [[3]]
#> [1] "2019-02-01" "2019-02-02"
#> 

# Or into 1-month periods
slide_period(i, i, "month", identity)
#> [[1]]
#> [1] "2019-01-28" "2019-01-29" "2019-01-30" "2019-01-31"
#> 
#> [[2]]
#> [1] "2019-02-01" "2019-02-02"
#> 

# Now select:
# - The current 2-day period
# - Plus 1 2-day period before the current one
slide_period(i, i, "day", identity, .every = 2, .before = 1)
#> [[1]]
#> [1] "2019-01-28" "2019-01-29"
#> 
#> [[2]]
#> [1] "2019-01-28" "2019-01-29" "2019-01-30" "2019-01-31"
#> 
#> [[3]]
#> [1] "2019-01-30" "2019-01-31" "2019-02-01" "2019-02-02"
#> 

# Alter the `origin` to control the reference date for
# how the 2-day groups are formed
origin <- as.Date("2019-01-29")
slide_period(i, i, "day", identity, .every = 2, .origin = origin)
#> [[1]]
#> [1] "2019-01-28"
#> 
#> [[2]]
#> [1] "2019-01-29" "2019-01-30"
#> 
#> [[3]]
#> [1] "2019-01-31" "2019-02-01"
#> 
#> [[4]]
#> [1] "2019-02-02"
#> 

# This can be useful for, say, monthly averages
daily_sales <- c(2, 5, 3, 6, 9, 4)
slide_period_dbl(daily_sales, i, "month", mean)
#> [1] 4.0 6.5

# If you need the index, slide over and return a data frame
sales_df <- data.frame(i = i, sales = daily_sales)

slide_period_dfr(
  sales_df,
  sales_df$i,
  "month",
  ~data.frame(
     i = max(.x$i),
     sales = mean(.x$sales)
   )
)
#>            i sales
#> 1 2019-01-31   4.0
#> 2 2019-02-02   6.5

# One of the most unique features about `slide_period()` is that it is
# aware of how far apart elements of `.i` are in the `.period` you are
# interested in. For example, if you do a monthly slide with `i2`, selecting
# the current month and 1 month before it, then it will recognize that
# `2019-02-01` and `2019-04-01` are not beside each other, and it won't
# group them together.
i2 <- as.Date(c("2019-01-01", "2019-02-01", "2019-04-01", "2019-05-01"))

slide_period(i2, i2, "month", identity, .before = 1)
#> [[1]]
#> [1] "2019-01-01"
#> 
#> [[2]]
#> [1] "2019-01-01" "2019-02-01"
#> 
#> [[3]]
#> [1] "2019-04-01"
#> 
#> [[4]]
#> [1] "2019-04-01" "2019-05-01"
#> 
```
