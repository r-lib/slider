# Slide along multiples inputs simultaneously relative to an index

`slide_index2()` and `pslide_index()` represent the combination of
[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md) and
[`pslide()`](https://slider.r-lib.org/dev/reference/slide2.md) with
[`slide_index()`](https://slider.r-lib.org/dev/reference/slide_index.md),
allowing you to iterate over multiple vectors at once relative to an
`.i`-ndex.

## Usage

``` r
slide_index2(.x, .y, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

slide_index2_vec(
  .x,
  .y,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .ptype = NULL
)

slide_index2_dbl(
  .x,
  .y,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_index2_int(
  .x,
  .y,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_index2_lgl(
  .x,
  .y,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_index2_chr(
  .x,
  .y,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE
)

slide_index2_dfr(
  .x,
  .y,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

slide_index2_dfc(
  .x,
  .y,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
)

pslide_index(.l, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

pslide_index_vec(
  .l,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .ptype = NULL
)

pslide_index_dbl(.l, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

pslide_index_int(.l, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

pslide_index_lgl(.l, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

pslide_index_chr(.l, .i, .f, ..., .before = 0L, .after = 0L, .complete = FALSE)

pslide_index_dfr(
  .l,
  .i,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

pslide_index_dfc(
  .l,
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

- .x, .y:

  `[vector]`

  Vectors to iterate over. Vectors of size 1 will be recycled.

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

- .l:

  `[list]`

  A list of vectors. The length of `.l` determines the number of
  arguments that `.f` will be called with. If `.l` has names, they will
  be used as named arguments to `.f`. Elements of `.l` with size 1 will
  be recycled.

## Value

A vector fulfilling the following invariants:

### `slide_index2()`

- `vec_size(slide_index2(.x, .y)) == vec_size_common(.x, .y)`

- `vec_ptype(slide_index2(.x, .y)) == list()`

### `slide_index2_vec()` and `slide_index2_*()` variants

- `vec_size(slide_index2_vec(.x, .y)) == vec_size_common(.x, .y)`

- `vec_size(slide_index2_vec(.x, .y)[[1]]) == 1L`

- `vec_ptype(slide_index2_vec(.x, .y, .ptype = ptype)) == ptype`

### `pslide_index()`

- `vec_size(pslide_index(.l)) == vec_size_common(!!! .l)`

- `vec_ptype(pslide_index(.l)) == list()`

### `pslide_index_vec()` and `pslide_index_*()` variants

- `vec_size(pslide_index_vec(.l)) == vec_size_common(!!! .l)`

- `vec_size(pslide_index_vec(.l)[[1]]) == 1L`

- `vec_ptype(pslide_index_vec(.l, .ptype = ptype)) == ptype`

## See also

[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md),
[`hop_index2()`](https://slider.r-lib.org/dev/reference/hop_index2.md),
[`slide_index()`](https://slider.r-lib.org/dev/reference/slide_index.md)

## Examples

``` r
# Notice that `i` is an irregular index!
x <- 1:5
y <- 6:10
i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)

# When we slide over `i` looking back 1 day, the irregularity is respected.
# When there is a gap in dates, only 2 values are returned (one from
# `x` and one from `y`), otherwise, 4 values are returned.
slide_index2(x, y, i, ~c(.x, .y), .before = 1)
#> [[1]]
#> [1] 1 6
#> 
#> [[2]]
#> [1] 1 2 6 7
#> 
#> [[3]]
#> [1] 3 8
#> 
#> [[4]]
#> [1] 4 9
#> 
#> [[5]]
#> [1]  4  5  9 10
#> 
```
