# Index arithmetic

`slider_plus()` and `slider_minus()` are developer functions used to
register special double dispatch methods to control how `.before` and
`.after` are subtracted from and added to `.i`. These allow developers
to overcome some of the restrictions around `+` and `-` when custom S3
types are involved. These should only be used by package authors
creating new index types.

- `slider_plus()` allows you to override the default behavior of
  `.i + .after`. When writing the S3 method, `x` will be `.i`, and `y`
  will be `.after`.

- `slider_minus()` allows you to override the default behavior of
  `.i - .before`. When writing the S3 method, `x` will be `.i`, and `y`
  will be `.before`.

These generics are a bit special. They work similarly to
[`vctrs::vec_ptype2()`](https://vctrs.r-lib.org/reference/vec_ptype2.html)
in that they are *double dispatch* methods that dispatch off the types
of both `x` and `y`. To write an S3 method for these generics, write and
export an S3 method of the form:

    slider_plus.x_class.y_class <- function(x, y) {
      # My method
    }

Inheritance is not considered in the method lookup, and you cannot use
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) from within your
method.

## Usage

``` r
slider_plus(x, y)

slider_minus(x, y)
```

## Arguments

- x, y:

  `[vector]`

  Two vectors to add or subtract.

  `x` will always be the index, `.i`.

  For `slider_plus()`, `y` will be `.after`.

  For `slider_minus()`, `y` will be `.before`.

## Value

- For `slider_plus()`, `x` after adding `y`.

- For `slider_minus()`, `x` after subtracting `y`.

The result should always be the same type and size as `x`.

## Examples

``` r
slider_plus(1, 2)
#> [1] 3
slider_minus(1, 2)
#> [1] -1
```
