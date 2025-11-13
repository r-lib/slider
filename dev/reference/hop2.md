# Hop along multiple inputs simultaneously

`hop2()` and `phop()` represent the combination of
[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md) and
[`pslide()`](https://slider.r-lib.org/dev/reference/slide2.md) with
[`hop()`](https://slider.r-lib.org/dev/reference/hop.md), allowing you
to iterate over multiple vectors at once, hopping along them using
boundaries defined by `.starts` and `.stops`.

## Usage

``` r
hop2(.x, .y, .starts, .stops, .f, ...)

hop2_vec(.x, .y, .starts, .stops, .f, ..., .ptype = NULL)

phop(.l, .starts, .stops, .f, ...)

phop_vec(.l, .starts, .stops, .f, ..., .ptype = NULL)
```

## Arguments

- .x, .y:

  `[vector]`

  Vectors to iterate over. Vectors of size 1 will be recycled.

- .starts, .stops:

  `[integer]`

  Vectors of boundary locations that make up the windows to bucket `.x`
  with. Both `.starts` and `.stops` will be recycled to their common
  size, and that common size will be the size of the result. Both
  vectors should be integer locations along `.x`, but out-of-bounds
  values are allowed.

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

- .ptype:

  `[vector(0) / NULL]`

  A prototype corresponding to the type of the output.

  If `NULL`, the default, the output type is determined by computing the
  common type across the results of the calls to `.f`.

  If supplied, the result of each call to `.f` will be cast to that
  type, and the final output will have that type.

  If `getOption("vctrs.no_guessing")` is `TRUE`, the `.ptype` must be
  supplied. This is a way to make production code demand fixed types.

- .l:

  `[list]`

  A list of vectors. The length of `.l` determines the number of
  arguments that `.f` will be called with. If `.l` has names, they will
  be used as named arguments to `.f`. Elements of `.l` with size 1 will
  be recycled.

## Value

A vector fulfilling the following invariants:

### `hop2()`

- `vec_size(hop2(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_ptype(hop2(.x, .y, .starts, .stops)) == list()`

### `hop2_vec()`

- `vec_size(hop2_vec(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_size(hop2_vec(.x, .y, .starts, .stops)[[1]]) == 1L`

- `vec_ptype(hop2_vec(.x, .y, .starts, .stops, .ptype = ptype)) == ptype`

### `phop()`

- `vec_size(phop(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_ptype(phop(.l, .starts, .stops)) == list()`

### `phop_vec()`

- `vec_size(phop_vec(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_size(phop_vec(.l, .starts, .stops)[[1]]) == 1L`

- `vec_ptype(phop_vec(.l, .starts, .stops, .ptype = ptype)) == ptype`

## See also

[`hop()`](https://slider.r-lib.org/dev/reference/hop.md),
[`hop_index()`](https://slider.r-lib.org/dev/reference/hop_index.md),
[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md)

## Examples

``` r
hop2(1:2, 3:4, .starts = 1, .stops = c(2, 1), ~c(x = .x, y = .y))
#> [[1]]
#> x1 x2 y1 y2 
#>  1  2  3  4 
#> 
#> [[2]]
#> x y 
#> 1 3 
#> 

phop(
 list(1, 2:4, 5:7),
 .starts = c(0, 1),
 .stops  = c(2, 4),
 ~c(x = ..1, y = ..2, z = ..3)
)
#> [[1]]
#> x1 x2 y1 y2 z1 z2 
#>  1  1  2  3  5  6 
#> 
#> [[2]]
#> x1 x2 x3 y1 y2 y3 z1 z2 z3 
#>  1  1  1  2  3  4  5  6  7 
#> 
```
