# Hop along multiple inputs simultaneously relative to an index

`hop_index2()` and `phop_index()` represent the combination of
[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md) and
[`pslide()`](https://slider.r-lib.org/dev/reference/slide2.md) with
[`hop_index()`](https://slider.r-lib.org/dev/reference/hop_index.md),
allowing you to iterate over multiple vectors at once, relative to an
`.i`-ndex with boundaries defined by `.starts` and `.stops`.

## Usage

``` r
hop_index2(.x, .y, .i, .starts, .stops, .f, ...)

hop_index2_vec(.x, .y, .i, .starts, .stops, .f, ..., .ptype = NULL)

phop_index(.l, .i, .starts, .stops, .f, ...)

phop_index_vec(.l, .i, .starts, .stops, .f, ..., .ptype = NULL)
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

- .starts, .stops:

  `[vector]`

  Vectors of boundary values that make up the windows to bucket `.i`
  with. Both `.starts` and `.stops` will be recycled to their common
  size, and that common size will be the size of the result. Both
  vectors will be cast to the type of `.i` using
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).
  These boundaries are both *inclusive*, meaning that the slice of `.x`
  that will be used in each call to `.f` is where
  `.i >= start & .i <= stop` returns `TRUE`.

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

### `hop_index2()`

- `vec_size(hop_index2(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_ptype(hop_index2(.x, .y, .starts, .stops)) == list()`

### `hop_index2_vec()`

- `vec_size(hop_index2_vec(.x, .y, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_size(hop_index2_vec(.x, .y, .starts, .stops)[[1]]) == 1L`

- `vec_ptype(hop_index2_vec(.x, .y, .starts, .stops, .ptype = ptype)) == ptype`

### `phop_index()`

- `vec_size(phop_index(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_ptype(phop_index(.l, .starts, .stops)) == list()`

### `phop_index_vec()`

- `vec_size(phop_index_vec(.l, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_size(phop_index_vec(.l, .starts, .stops)[[1]]) == 1L`

- `vec_ptype(phop_index_vec(.l, .starts, .stops, .ptype = ptype)) == ptype`

## See also

[`slide2()`](https://slider.r-lib.org/dev/reference/slide2.md),
[`slide_index2()`](https://slider.r-lib.org/dev/reference/slide_index2.md),
[`hop_index()`](https://slider.r-lib.org/dev/reference/hop_index.md)

## Examples

``` r
# Notice that `i` is an irregular index!
x <- 1:5
i <- as.Date("2019-08-15") + c(0:1, 4, 6, 7)

# Manually create starts/stops. They don't have to be equally spaced,
# and they don't have to be the same size as `.x` or `.i`.
starts <- as.Date(c("2019-08-15", "2019-08-18"))
stops <- as.Date(c("2019-08-16", "2019-08-23"))

# The output size is equal to the common size of `.starts` and `.stops`
hop_index2(x, i, i, starts, stops, ~data.frame(x = .x, y = .y))
#> [[1]]
#>   x          y
#> 1 1 2019-08-15
#> 2 2 2019-08-16
#> 
#> [[2]]
#>   x          y
#> 1 3 2019-08-19
#> 2 4 2019-08-21
#> 3 5 2019-08-22
#> 
```
