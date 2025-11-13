# Slide along multiple inputs simultaneously relative to an index chunked by period

`slide_period2()` and `pslide_period()` represent the combination of
[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md) and
[`pslide()`](https://slider.r-lib.org/dev/reference/slide2.md) with
[`slide_period()`](https://slider.r-lib.org/dev/reference/slide_period.md),
allowing you to slide over multiple vectors at once, using indices
defined by breaking up the `.i`-ndex by `.period`.

## Usage

``` r
slide_period2(
  .x,
  .y,
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

slide_period2_vec(
  .x,
  .y,
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

slide_period2_dbl(
  .x,
  .y,
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

slide_period2_int(
  .x,
  .y,
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

slide_period2_lgl(
  .x,
  .y,
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

slide_period2_chr(
  .x,
  .y,
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

slide_period2_dfr(
  .x,
  .y,
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

slide_period2_dfc(
  .x,
  .y,
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

pslide_period(
  .l,
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

pslide_period_vec(
  .l,
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

pslide_period_dbl(
  .l,
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

pslide_period_int(
  .l,
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

pslide_period_lgl(
  .l,
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

pslide_period_chr(
  .l,
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

pslide_period_dfr(
  .l,
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

pslide_period_dfc(
  .l,
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

- .x, .y:

  `[vector]`

  Vectors to iterate over. Vectors of size 1 will be recycled.

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

- .l:

  `[list]`

  A list of vectors. The length of `.l` determines the number of
  arguments that `.f` will be called with. If `.l` has names, they will
  be used as named arguments to `.f`. Elements of `.l` with size 1 will
  be recycled.

## Value

A vector fulfilling the following invariants:

### `slide_period2()`

- `vec_size(slide_period2(.x, .y)) == vec_size(unique(warp::warp_distance(.i)))`

- `vec_ptype(slide_period2(.x, .y)) == list()`

### `slide_period2_vec()` and `slide_period2_*()` variants

- `vec_size(slide_period2_vec(.x, .y)) == vec_size(unique(warp::warp_distance(.i)))`

- `vec_size(slide_period2_vec(.x, .y)[[1]]) == 1L`

- `vec_ptype(slide_period2_vec(.x, .y, .ptype = ptype)) == ptype`

### `pslide_period()`

- `vec_size(pslide_period(.l)) == vec_size(unique(warp::warp_distance(.i)))`

- `vec_ptype(pslide_period(.l)) == list()`

### `pslide_period_vec()` and `pslide_period_*()` variants

- `vec_size(pslide_period_vec(.l)) == vec_size(unique(warp::warp_distance(.i)))`

- `vec_size(pslide_period_vec(.l)[[1]]) == 1L`

- `vec_ptype(pslide_period_vec(.l, .ptype = ptype)) == ptype`

## See also

[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md),
[`slide_index2()`](https://slider.r-lib.org/dev/reference/slide_index2.md),
[`slide_period()`](https://slider.r-lib.org/dev/reference/slide_period.md)

## Examples

``` r
i <- as.Date("2019-01-28") + 0:5

slide_period2(
  .x = 1:6,
  .y = i,
  .i = i,
  .period = "month",
  .f = ~data.frame(x = .x, i = .y)
)
#> [[1]]
#>   x          i
#> 1 1 2019-01-28
#> 2 2 2019-01-29
#> 3 3 2019-01-30
#> 4 4 2019-01-31
#> 
#> [[2]]
#>   x          i
#> 1 5 2019-02-01
#> 2 6 2019-02-02
#> 

pslide_period(
  .l = list(1:6, 7:12, i),
  .i = i,
  .period = "month",
  .f = ~data.frame(x = .x, y = .y, i = ..3)
)
#> [[1]]
#>   x  y          i
#> 1 1  7 2019-01-28
#> 2 2  8 2019-01-29
#> 3 3  9 2019-01-30
#> 4 4 10 2019-01-31
#> 
#> [[2]]
#>   x  y          i
#> 1 5 11 2019-02-01
#> 2 6 12 2019-02-02
#> 
```
