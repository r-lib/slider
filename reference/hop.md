# Hop

`hop()` is the lower level engine that powers
[`slide()`](https://slider.r-lib.org/reference/slide.md) (at least in
theory). It has slightly different invariants than
[`slide()`](https://slider.r-lib.org/reference/slide.md), and is useful
when you either need to hand craft boundary locations, or want to
compute a result with a size that is different from `.x`.

## Usage

``` r
hop(.x, .starts, .stops, .f, ...)

hop_vec(.x, .starts, .stops, .f, ..., .ptype = NULL)
```

## Arguments

- .x:

  `[vector]`

  The vector to iterate over and apply `.f` to.

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

## Value

A vector fulfilling the following invariants:

### `hop()`

- `vec_size(hop(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_ptype(hop(.x, .starts, .stops)) == list()`

### `hop_vec()`

- `vec_size(hop_vec(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_size(hop_vec(.x, .starts, .stops)[[1]]) == 1L`

- `vec_ptype(hop_vec(.x, .starts, .stops, .ptype = ptype)) == ptype`

## Details

`hop()` is very close to being a faster version of:

    map2(
      .starts,
      .stops,
      function(start, stop) {
        x_slice <- vec_slice(.x, start:stop)
        .f(x_slice, ...)
      }
    )

Because of this,
[`hop_index()`](https://slider.r-lib.org/reference/hop_index.md) is
often the more useful function. `hop()` mainly exists for API
completeness.

The main difference is that the start and stop values make up ranges of
*possible* locations along `.x`, and it is not enforced that these
locations actually exist along `.x`. As an example, with `hop()` you can
do the following, which would be an error with `vec_slice()` because
`0L` is out of bounds.

    hop(c("a", "b"), .starts = 0L, .stops = 1L, ~.x)
    #> [[1]]
    #> [1] "a"

`hop()` allows these out of bounds values to be fully compatible with
[`slide()`](https://slider.r-lib.org/reference/slide.md). It is always
possible to construct a `hop()` call from a
[`slide()`](https://slider.r-lib.org/reference/slide.md) call. For
example, the following are equivalent:

    slide(1:2, ~.x, .before = 1)

    hop(1:2, .starts = c(0, 1), .stops = c(1, 2), ~.x)

    #> [[1]]
    #> [1] 1
    #>
    #> [[2]]
    #> [1] 1 2

## See also

[`hop2()`](https://slider.r-lib.org/reference/hop2.md),
[`hop_index()`](https://slider.r-lib.org/reference/hop_index.md),
[`slide()`](https://slider.r-lib.org/reference/slide.md)

## Examples

``` r
# `hop()` let's you manually specify locations to apply `.f` at.
hop(1:3, .starts = c(1, 3), .stops = 3, ~.x)
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 3
#> 

# `hop()`'s start/stop locations are allowed to be out of bounds relative
# to the size of `.x`.
hop(
  mtcars,
  .starts = c(-1, 3),
  .stops  = c(2, 6),
  ~.x
)
#> [[1]]
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> 
#> [[2]]
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#> 
```
