# Slide over multiple inputs simultaneously

These are variants of
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) that
iterate over multiple inputs in parallel. They are parallel in the sense
that each input is processed in parallel with the others, not in the
sense of multicore computing. These functions work similarly to `map2()`
and `pmap()` from purrr.

## Usage

``` r
slide2(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide2_vec(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .ptype = NULL
)

slide2_dbl(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide2_int(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide2_lgl(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide2_chr(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide2_dfr(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

slide2_dfc(
  .x,
  .y,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
)

pslide(.l, .f, ..., .before = 0L, .after = 0L, .step = 1L, .complete = FALSE)

pslide_vec(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .ptype = NULL
)

pslide_dbl(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

pslide_int(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

pslide_lgl(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

pslide_chr(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

pslide_dfr(
  .l,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

pslide_dfc(
  .l,
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

- .x, .y:

  `[vector]`

  Vectors to iterate over. Vectors of size 1 will be recycled.

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

- .l:

  `[list]`

  A list of vectors. The length of `.l` determines the number of
  arguments that `.f` will be called with. If `.l` has names, they will
  be used as named arguments to `.f`. Elements of `.l` with size 1 will
  be recycled.

## Value

A vector fulfilling the following invariants:

### `slide2()`

- `vec_size(slide2(.x, .y)) == vec_size_common(.x, .y)`

- `vec_ptype(slide2(.x, .y)) == list()`

### `slide2_vec()` and `slide2_*()` variants

- `vec_size(slide2_vec(.x, .y)) == vec_size_common(.x, .y)`

- `vec_size(slide2_vec(.x, .y)[[1]]) == 1L`

- `vec_ptype(slide2_vec(.x, .y, .ptype = ptype)) == ptype`

### `pslide()`

- `vec_size(pslide(.l)) == vec_size_common(!!! .l)`

- `vec_ptype(pslide(.l)) == list()`

### `pslide_vec()` and `pslide_*()` variants

- `vec_size(pslide_vec(.l)) == vec_size_common(!!! .l)`

- `vec_size(pslide_vec(.l)[[1]]) == 1L`

- `vec_ptype(pslide_vec(.l, .ptype = ptype)) == ptype`

## See also

[`slide()`](https://slider.r-lib.org/dev/reference/slide.md),
[`slide_index2()`](https://slider.r-lib.org/dev/reference/slide_index2.md),
[`hop_index2()`](https://slider.r-lib.org/dev/reference/hop_index2.md)

## Examples

``` r
# Slide along two inputs at once
slide2(1:4, 5:8, ~list(.x, .y), .before = 2)
#> [[1]]
#> [[1]][[1]]
#> [1] 1
#> 
#> [[1]][[2]]
#> [1] 5
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] 1 2
#> 
#> [[2]][[2]]
#> [1] 5 6
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] 1 2 3
#> 
#> [[3]][[2]]
#> [1] 5 6 7
#> 
#> 
#> [[4]]
#> [[4]][[1]]
#> [1] 2 3 4
#> 
#> [[4]][[2]]
#> [1] 6 7 8
#> 
#> 

# Or, for more than two, use `pslide()`
pslide(list(1:4, 5:8, 9:12), ~list(.x, .y, ..3), .before = 2)
#> [[1]]
#> [[1]][[1]]
#> [1] 1
#> 
#> [[1]][[2]]
#> [1] 5
#> 
#> [[1]][[3]]
#> [1] 9
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] 1 2
#> 
#> [[2]][[2]]
#> [1] 5 6
#> 
#> [[2]][[3]]
#> [1]  9 10
#> 
#> 
#> [[3]]
#> [[3]][[1]]
#> [1] 1 2 3
#> 
#> [[3]][[2]]
#> [1] 5 6 7
#> 
#> [[3]][[3]]
#> [1]  9 10 11
#> 
#> 
#> [[4]]
#> [[4]][[1]]
#> [1] 2 3 4
#> 
#> [[4]][[2]]
#> [1] 6 7 8
#> 
#> [[4]][[3]]
#> [1] 10 11 12
#> 
#> 

# You can even slide along the rows of multiple data frames of
# equal size at once
set.seed(16)
x <- data.frame(a = rnorm(5), b = rnorm(5))
y <- data.frame(c = letters[1:5], d = letters[6:10])

row_return <- function(x_rows, y_rows) {
  if (sum(x_rows$a) < 0) {
    x_rows
  } else {
    y_rows
  }
}

slide2(x, y, row_return, .before = 1, .after = 2)
#> [[1]]
#>   c d
#> 1 a f
#> 2 b g
#> 3 c h
#> 
#> [[2]]
#>   c d
#> 1 a f
#> 2 b g
#> 3 c h
#> 4 d i
#> 
#> [[3]]
#>   c d
#> 1 b g
#> 2 c h
#> 3 d i
#> 4 e j
#> 
#> [[4]]
#>   c d
#> 1 c h
#> 2 d i
#> 3 e j
#> 
#> [[5]]
#>           a        b
#> 1 -1.444229 1.024973
#> 2  1.147829 0.573142
#> 
```
