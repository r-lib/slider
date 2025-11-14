# Slide relative to an index

`slide_index()` is similar to
[`slide()`](https://slider.r-lib.org/reference/slide.md), but allows a
secondary `.i`-ndex vector to be provided.

This is often useful in business calculations, when you want to compute
a rolling computation looking "3 months back", which is approximately
but not equivalent to, 3 \* 30 days. `slide_index()` allows for these
irregular window sizes.

## Usage

``` r
slide_index(.x, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

slide_index_vec(
  .x,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .ptype = NULL
)

slide_index_dbl(.x, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

slide_index_int(.x, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

slide_index_lgl(.x, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

slide_index_chr(.x, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

slide_index_dfr(
  .x,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

slide_index_dfc(
  .x,
  .i,
  .f,
  ...,
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

  `[vector]`

  The index vector that determines the window sizes. It is fairly common
  to supply a date vector as the index, but not required.

  There are 3 restrictions on the index:

  - The size of the index must match the size of `.x`, they will not be
    recycled to their common size.

  - The index must be an *increasing* vector, but duplicate values are
    allowed.

  - The index cannot have missing values.

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

- .before, .after:

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

### `slide_index()`

- `vec_size(slide_index(.x)) == vec_size(.x)`

- `vec_ptype(slide_index(.x)) == list()`

### `slide_index_vec()` and `slide_index_*()` variants

- `vec_size(slide_index_vec(.x)) == vec_size(.x)`

- `vec_size(slide_index_vec(.x)[[1]]) == 1L`

- `vec_ptype(slide_index_vec(.x, .ptype = ptype)) == ptype`

## See also

[`slide()`](https://slider.r-lib.org/reference/slide.md),
[`hop_index()`](https://slider.r-lib.org/reference/hop_index.md),
[`slide_index2()`](https://slider.r-lib.org/reference/slide_index2.md)

## Examples

``` r
library(lubridate)

x <- 1:5

# In some cases, sliding over `x` with a strict window size of 2
# will fit your use case.
slide(x, ~.x, .before = 1)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 2 3
#> 
#> [[4]]
#> [1] 3 4
#> 
#> [[5]]
#> [1] 4 5
#> 

# However, if this `i` is a date vector paired with `x`, when computing
# rolling calculations you might want to iterate over `x` while
# respecting the fact that `i` is an irregular sequence.
i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)

# For example, a "2 day" window should not pair `"2019-08-19"` and
# `"2019-08-21"` together, even though they are next to each other in `x`.
# `slide_index()` computes the lookback value from the current date in `.i`,
# meaning that if you are currently on `"2019-08-21"` and look back 1 day,
# it will correctly not include `"2019-08-19"`.
slide_index(i, i, ~.x, .before = 1)
#> [[1]]
#> [1] "2019-08-15"
#> 
#> [[2]]
#> [1] "2019-08-15" "2019-08-16"
#> 
#> [[3]]
#> [1] "2019-08-19"
#> 
#> [[4]]
#> [1] "2019-08-21"
#> 
#> [[5]]
#> [1] "2019-08-21" "2019-08-22"
#> 

# We could have equivalently used a lubridate period object for this as well,
# since `i - lubridate::days(1)` is allowed
slide_index(i, i, ~.x, .before = lubridate::days(1))
#> [[1]]
#> [1] "2019-08-15"
#> 
#> [[2]]
#> [1] "2019-08-15" "2019-08-16"
#> 
#> [[3]]
#> [1] "2019-08-19"
#> 
#> [[4]]
#> [1] "2019-08-21"
#> 
#> [[5]]
#> [1] "2019-08-21" "2019-08-22"
#> 

# ---------------------------------------------------------------------------
# Functions for `.before` and `.after`

# In some cases, it might not be appropriate to compute
# `.i - .before` or `.i + .after`, either because there isn't a `-` or `+`
# method defined, or because there is an alternative way to perform the
# arithmetic. For example, subtracting 1 month with `- months(1)` (using
# lubridate) can sometimes land you on an invalid date that doesn't exist.
i <- as.Date(c("2019-01-31", "2019-02-28", "2019-03-31"))

# 2019-03-31 - months(1) = 2019-02-31, which doesn't exist
i - months(1)
#> [1] "2018-12-31" "2019-01-28" NA          

# These NAs create problems with `slide_index()`, which doesn't allow
# missing values in the computed endpoints
try(slide_index(i, i, identity, .before = months(1)))
#> Error in slide_index(i, i, identity, .before = months(1)) : 
#>   ℹ In locations: 3
#> ! Endpoints generated by `.before` can't be `NA`.

# In these cases, it is more appropriate to use `%m-%`,
# which will snap to the end of the month, at least giving you something
# to work with.
i %m-% months(1)
#> [1] "2018-12-31" "2019-01-28" "2019-02-28"

# To use this as your `.before` or `.after`, supply an anonymous function of
# 1 argument that performs the computation
slide_index(i, i, identity, .before = ~.x %m-% months(1))
#> [[1]]
#> [1] "2019-01-31"
#> 
#> [[2]]
#> [1] "2019-01-31" "2019-02-28"
#> 
#> [[3]]
#> [1] "2019-02-28" "2019-03-31"
#> 

# Notice that in the `.after` case, `2019-02-28 %m+% months(1)` doesn't
# capture the end of March, so it isn't included in the 2nd result
slide_index(i, i, identity, .after = ~.x %m+% months(1))
#> [[1]]
#> [1] "2019-01-31" "2019-02-28"
#> 
#> [[2]]
#> [1] "2019-02-28"
#> 
#> [[3]]
#> [1] "2019-03-31"
#> 

# ---------------------------------------------------------------------------

# When `.i` has repeated values, they are always grouped together.
i <- c(2017, 2017, 2018, 2019, 2020, 2020)
slide_index(i, i, ~.x)
#> [[1]]
#> [1] 2017 2017
#> 
#> [[2]]
#> [1] 2017 2017
#> 
#> [[3]]
#> [1] 2018
#> 
#> [[4]]
#> [1] 2019
#> 
#> [[5]]
#> [1] 2020 2020
#> 
#> [[6]]
#> [1] 2020 2020
#> 
slide_index(i, i, ~.x, .after = 1)
#> [[1]]
#> [1] 2017 2017 2018
#> 
#> [[2]]
#> [1] 2017 2017 2018
#> 
#> [[3]]
#> [1] 2018 2019
#> 
#> [[4]]
#> [1] 2019 2020 2020
#> 
#> [[5]]
#> [1] 2020 2020
#> 
#> [[6]]
#> [1] 2020 2020
#> 

# ---------------------------------------------------------------------------
# Rolling regressions

# Rolling regressions are easy with `slide_index()` because:
# - Data frame `.x` values are iterated over rowwise
# - The index is respected by using `.i`
set.seed(123)

df <- data.frame(
  y = rnorm(100),
  x = rnorm(100),
  i = as.Date("2019-08-15") + c(0, 2, 4, 6:102) # <- irregular
)

# 20 day rolling regression. Current day + 19 days back.
# Additionally, set `.complete = TRUE` to not compute partial results.
regr <- slide_index(df, df$i, ~lm(y ~ x, .x), .before = 19, .complete = TRUE)

regr[16:18]
#> [[1]]
#> NULL
#> 
#> [[2]]
#> 
#> Call:
#> lm(formula = y ~ x, data = .x)
#> 
#> Coefficients:
#> (Intercept)            x  
#>      0.3257       0.2067  
#> 
#> 
#> [[3]]
#> 
#> Call:
#> lm(formula = y ~ x, data = .x)
#> 
#> Coefficients:
#> (Intercept)            x  
#>      0.2574       0.2632  
#> 
#> 

# The first 16 slots are NULL because there is no possible way to
# look back 19 days from the 16th index position and construct a full
# window. But on the 17th index position, `""2019-09-03"`, if we look
# back 19 days we get to `""2019-08-15"`, which is the same value as
# `i[1]` so a full window can be constructed.
df$i[16] - 19 >= df$i[1] # FALSE
#> [1] FALSE
df$i[17] - 19 >= df$i[1] # TRUE
#> [1] TRUE

# ---------------------------------------------------------------------------
# Accessing the current index value

# A very simplistic version of `purrr::map2()`
fake_map2 <- function(.x, .y, .f, ...) {
  Map(.f, .x, .y, ...)
}

# Occasionally you need to access the index value that you are currently on.
# This is generally not possible with a single call to `slide_index()`, but
# can be easily accomplished by following up a `slide_index()` call with a
# `purrr::map2()`. In this example, we want to use the distance from the
# current index value (in days) as a multiplier on `x`. Values further
# away from the current date get a higher multiplier.
set.seed(123)

# 25 random days past 2000-01-01
i <- sort(as.Date("2000-01-01") + sample(100, 25))

df <- data.frame(i = i, x = rnorm(25))

weight_by_distance <- function(df, i) {
  df$weight = abs(as.integer(df$i - i))
  df$x_weighted = df$x * df$weight
  df
}

# Use `slide_index()` to just generate the rolling data.
# Here we take the current date + 5 days before + 5 days after.
dfs <- slide_index(df, df$i, ~.x, .before = 5, .after = 5)

# Follow up with a `map2()` with `i` as the second input.
# This allows you to track the current `i` value and weight accordingly.
result <- fake_map2(dfs, df$i, weight_by_distance)

head(result)
#> [[1]]
#>            i          x weight x_weighted
#> 1 2000-01-08 -0.2179749      0   0.000000
#> 2 2000-01-10 -1.0260044      2  -2.052009
#> 
#> [[2]]
#>            i          x weight x_weighted
#> 1 2000-01-08 -0.2179749      2 -0.4359498
#> 2 2000-01-10 -1.0260044      0  0.0000000
#> 3 2000-01-15 -0.7288912      5 -3.6444561
#> 
#> [[3]]
#>            i          x weight x_weighted
#> 1 2000-01-10 -1.0260044      5 -5.1300222
#> 2 2000-01-15 -0.7288912      0  0.0000000
#> 3 2000-01-16 -0.6250393      1 -0.6250393
#> 
#> [[4]]
#>            i          x weight x_weighted
#> 1 2000-01-15 -0.7288912      1 -0.7288912
#> 2 2000-01-16 -0.6250393      0  0.0000000
#> 
#> [[5]]
#>            i         x weight x_weighted
#> 1 2000-01-26 -1.686693      0   0.000000
#> 2 2000-01-27  0.837787      1   0.837787
#> 
#> [[6]]
#>            i          x weight x_weighted
#> 1 2000-01-26 -1.6866933      1 -1.6866933
#> 2 2000-01-27  0.8377870      0  0.0000000
#> 3 2000-02-01  0.1533731      5  0.7668656
#> 
```
