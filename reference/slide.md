# Slide

`slide()` iterates through `.x` using a sliding window, applying `.f` to
each sub-window of `.x`.

## Usage

``` r
slide(.x, .f, ..., .before = 0L, .after = 0L, .step = 1L, .complete = FALSE)

slide_vec(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .ptype = NULL
)

slide_dbl(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_int(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_lgl(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_chr(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_dfr(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

slide_dfc(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
)
```

## Arguments

- .x:

  `[vector]`

  The vector to iterate over and apply `.f` to.

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

  `[integer(1) / Inf]`

  The number of values before or after the current element to include in
  the sliding window. Set to `Inf` to select all elements before or
  after the current element. Negative values are allowed, which allows
  you to "look forward" from the current element if used as the
  `.before` value, or "look backwards" if used as `.after`.

- .step:

  `[positive integer(1)]`

  The number of elements to shift the window forward between function
  calls.

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

### `slide()`

- `vec_size(slide(.x)) == vec_size(.x)`

- `vec_ptype(slide(.x)) == list()`

### `slide_vec()` and `slide_*()` variants

- `vec_size(slide_vec(.x)) == vec_size(.x)`

- `vec_size(slide_vec(.x)[[1]]) == 1L`

- `vec_ptype(slide_vec(.x, .ptype = ptype)) == ptype`

## Details

Unlike [`lapply()`](https://rdrr.io/r/base/lapply.html) or
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html), which
construct calls like

    .f(.x[[i]], ...)

the equivalent with `slide()` looks like

    .f(vctrs::vec_slice(.x, i), ...)

which is approximately

    .f(.x[i], ...)

except in the case of data frames or arrays, which are iterated over
row-wise.

If `.x` has names, then the output will preserve those names.

Using
[`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html),
the output of `.f` will be automatically cast to the type required by
the variant of `slide_*()` being used.

## See also

[`slide2()`](https://slider.r-lib.org/reference/slide2.md),
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md),
[`hop()`](https://slider.r-lib.org/reference/hop.md)

## Examples

``` r
# The defaults work similarly to `map()`
slide(1:5, ~.x)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
#> 
#> [[5]]
#> [1] 5
#> 

# Use `.before`, `.after`, and `.step` to control the window
slide(1:5, ~.x, .before = 1)
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

# This can be used for rolling means
slide_dbl(rnorm(5), mean, .before = 2)
#> [1] -0.05260191  0.24519722 -0.14122680  0.03235865 -0.02765638

# Or more flexible rolling operations
slide(rnorm(5), ~ .x - mean(.x), .before = 2)
#> [[1]]
#> [1] 0
#> 
#> [[2]]
#> [1] -1.02116  1.02116
#> 
#> [[3]]
#> [1] -1.7451228  0.2971971  1.4479257
#> 
#> [[4]]
#> [1] -0.1051691  1.0455595 -0.9403905
#> 
#> [[5]]
#> [1]  1.6034341 -0.3825159 -1.2209182
#> 

# `.after` allows you to "align to the left" rather than the right
slide(1:5, ~.x, .after = 2)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 2 3 4
#> 
#> [[3]]
#> [1] 3 4 5
#> 
#> [[4]]
#> [1] 4 5
#> 
#> [[5]]
#> [1] 5
#> 

# And a mixture of `.before` and `.after`
# allows you complete control over the exact alignment.
# Below, "center alignment" is used.
slide(1:5, ~.x, .before = 1, .after = 1)
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1 2 3
#> 
#> [[3]]
#> [1] 2 3 4
#> 
#> [[4]]
#> [1] 3 4 5
#> 
#> [[5]]
#> [1] 4 5
#> 

# The `.step` controls how the window is shifted along `.x`,
# allowing you to "skip" iterations if you only need a less granular result
slide(1:10, ~.x, .before = 2, .step = 3)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> NULL
#> 
#> [[4]]
#> [1] 2 3 4
#> 
#> [[5]]
#> NULL
#> 
#> [[6]]
#> NULL
#> 
#> [[7]]
#> [1] 5 6 7
#> 
#> [[8]]
#> NULL
#> 
#> [[9]]
#> NULL
#> 
#> [[10]]
#> [1]  8  9 10
#> 

# `.complete` controls whether or not partial results are computed.
# By default, they are, but setting `.complete = TRUE` restricts
# `slide()` to only evaluate the function where a complete window exists.
slide(1:5, ~.x, .before = 2, .after = 1)
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1 2 3
#> 
#> [[3]]
#> [1] 1 2 3 4
#> 
#> [[4]]
#> [1] 2 3 4 5
#> 
#> [[5]]
#> [1] 3 4 5
#> 
slide(1:5, ~.x, .before = 2, .after = 1, .complete = TRUE)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> [1] 1 2 3 4
#> 
#> [[4]]
#> [1] 2 3 4 5
#> 
#> [[5]]
#> NULL
#> 

# ---------------------------------------------------------------------------
# Data frames

# Data frames are iterated over rowwise
mtcars_rowwise <- slide(mtcars, ~.x)
mtcars_rowwise[1:3]
#> $`Mazda RX4`
#>           mpg cyl disp  hp drat   wt  qsec vs am gear carb
#> Mazda RX4  21   6  160 110  3.9 2.62 16.46  0  1    4    4
#> 
#> $`Mazda RX4 Wag`
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> 
#> $`Datsun 710`
#>             mpg cyl disp hp drat   wt  qsec vs am gear carb
#> Datsun 710 22.8   4  108 93 3.85 2.32 18.61  1  1    4    1
#> 

# This means that any column name is easily accessible
slide_dbl(mtcars, ~.x$mpg + .x$cyl)
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710 
#>                27.0                27.0                26.8 
#>      Hornet 4 Drive   Hornet Sportabout             Valiant 
#>                27.4                26.7                24.1 
#>          Duster 360           Merc 240D            Merc 230 
#>                22.3                28.4                26.8 
#>            Merc 280           Merc 280C          Merc 450SE 
#>                25.2                23.8                24.4 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood 
#>                25.3                23.2                18.4 
#> Lincoln Continental   Chrysler Imperial            Fiat 128 
#>                18.4                22.7                36.4 
#>         Honda Civic      Toyota Corolla       Toyota Corona 
#>                34.4                37.9                25.5 
#>    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                23.5                23.2                21.3 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2 
#>                27.2                31.3                30.0 
#>        Lotus Europa      Ford Pantera L        Ferrari Dino 
#>                34.4                23.8                25.7 
#>       Maserati Bora          Volvo 142E 
#>                23.0                25.4 

# More advanced rowwise iteration is available as well by using the
# other arguments
mtcars_rowwise_window <- slide(mtcars, ~.x, .before = 1, .after = 1)
mtcars_rowwise_window[1:3]
#> $`Mazda RX4`
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> 
#> $`Mazda RX4 Wag`
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 
#> $`Datsun 710`
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710     22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> 

# ---------------------------------------------------------------------------
# Cumulative sliding

# Using the special cased value, `Inf`, you can ask `slide()` to pin the
# start of the sliding window to the first element, effectively creating
# a cumulative window
slide(1:5, ~.x, .before = Inf)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 1 2 3
#> 
#> [[4]]
#> [1] 1 2 3 4
#> 
#> [[5]]
#> [1] 1 2 3 4 5
#> 

# Same with `.after`, this creates a window where you start with all of the
# elements, but decrease the total number over each iteration
slide(1:5, ~.x, .after = Inf)
#> [[1]]
#> [1] 1 2 3 4 5
#> 
#> [[2]]
#> [1] 2 3 4 5
#> 
#> [[3]]
#> [1] 3 4 5
#> 
#> [[4]]
#> [1] 4 5
#> 
#> [[5]]
#> [1] 5
#> 

# ---------------------------------------------------------------------------
# Negative `.before` / `.after`

# `.before` is allowed to be negative, allowing you to "look forward" in
# your vector. Note that `abs(.before) <= .after` must hold if `.before` is
# negative. In this example, we look forward to elements in locations 2 and 3
# but place the result in position 1 in the output.
slide(1:5, ~.x, .before = -1, .after = 2)
#> [[1]]
#> [1] 2 3
#> 
#> [[2]]
#> [1] 3 4
#> 
#> [[3]]
#> [1] 4 5
#> 
#> [[4]]
#> [1] 5
#> 
#> [[5]]
#> integer(0)
#> 

# `.after` can be negative as well to "look backwards"
slide(1:5, ~.x, .before = 2, .after = -1)
#> [[1]]
#> integer(0)
#> 
#> [[2]]
#> [1] 1
#> 
#> [[3]]
#> [1] 1 2
#> 
#> [[4]]
#> [1] 2 3
#> 
#> [[5]]
#> [1] 3 4
#> 

# ---------------------------------------------------------------------------
# Removing padding

# If you are looking for a way to remove the `NA` values from something like
# this, then it doesn't exist as a built in option.
x <- rnorm(10)
slide_dbl(x, mean, .before = 3, .step = 2, .complete = TRUE)
#>  [1]          NA          NA          NA -0.35494393          NA
#>  [6] -0.03921560          NA  0.53894026          NA -0.07712438

# Adding an option to `slide_dbl()` to remove the `NA` values would destroy
# its size stability. Instead, you can use a combination of `slide_dfr()`
# to get the start/stop indices with `hop_index_vec()`.
i <- seq_along(x)
idx <- slide_dfr(
  i,
  ~data.frame(start = .x[1], stop = .x[length(.x)]),
  .before = 3,
  .step = 2,
  .complete = TRUE
)

idx
#>   start stop
#> 1     1    4
#> 2     3    6
#> 3     5    8
#> 4     7   10

hop_index_vec(x, i, idx$start, idx$stop, mean, .ptype = double())
#> [1] -0.35494393 -0.03921560  0.53894026 -0.07712438
```
