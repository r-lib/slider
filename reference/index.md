# Package index

## Slide family

The `slide(.x, .f)` functions are similar to `purrr::map(.x, .f)`. Both
iterate over `.x`, applying `.f` as they go. The difference is that
[`slide()`](https://slider.r-lib.org/reference/slide.md) moves over `.x`
in sliding windows, rather than one element at a time. The return value
size is always the same size as `.x`, and the type is defined by the
function suffix.

- [`slide()`](https://slider.r-lib.org/reference/slide.md)
  [`slide_vec()`](https://slider.r-lib.org/reference/slide.md)
  [`slide_dbl()`](https://slider.r-lib.org/reference/slide.md)
  [`slide_int()`](https://slider.r-lib.org/reference/slide.md)
  [`slide_lgl()`](https://slider.r-lib.org/reference/slide.md)
  [`slide_chr()`](https://slider.r-lib.org/reference/slide.md)
  [`slide_dfr()`](https://slider.r-lib.org/reference/slide.md)
  [`slide_dfc()`](https://slider.r-lib.org/reference/slide.md) : Slide
- [`slide2()`](https://slider.r-lib.org/reference/slide2.md)
  [`slide2_vec()`](https://slider.r-lib.org/reference/slide2.md)
  [`slide2_dbl()`](https://slider.r-lib.org/reference/slide2.md)
  [`slide2_int()`](https://slider.r-lib.org/reference/slide2.md)
  [`slide2_lgl()`](https://slider.r-lib.org/reference/slide2.md)
  [`slide2_chr()`](https://slider.r-lib.org/reference/slide2.md)
  [`slide2_dfr()`](https://slider.r-lib.org/reference/slide2.md)
  [`slide2_dfc()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide_vec()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide_dbl()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide_int()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide_lgl()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide_chr()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide_dfr()`](https://slider.r-lib.org/reference/slide2.md)
  [`pslide_dfc()`](https://slider.r-lib.org/reference/slide2.md) : Slide
  over multiple inputs simultaneously
- [`slide_sum()`](https://slider.r-lib.org/reference/summary-slide.md)
  [`slide_prod()`](https://slider.r-lib.org/reference/summary-slide.md)
  [`slide_mean()`](https://slider.r-lib.org/reference/summary-slide.md)
  [`slide_min()`](https://slider.r-lib.org/reference/summary-slide.md)
  [`slide_max()`](https://slider.r-lib.org/reference/summary-slide.md)
  [`slide_all()`](https://slider.r-lib.org/reference/summary-slide.md)
  [`slide_any()`](https://slider.r-lib.org/reference/summary-slide.md) :
  Specialized sliding functions

## Slide index family

These functions iterate over `.x` using sliding windows defined with a
secondary index vector, `.i`. The windows are constructed by “looking
back” and “looking forward” a certain number of periods from each
element of `.i`. This is useful for constructing irregular sliding
windows, such as sliding with a look back period of two months when you
have daily data. In that example, not all months have the same number of
days, and some months might be missing data, so a fixed window size
wouldn’t be useful.

- [`slide_index()`](https://slider.r-lib.org/reference/slide_index.md)
  [`slide_index_vec()`](https://slider.r-lib.org/reference/slide_index.md)
  [`slide_index_dbl()`](https://slider.r-lib.org/reference/slide_index.md)
  [`slide_index_int()`](https://slider.r-lib.org/reference/slide_index.md)
  [`slide_index_lgl()`](https://slider.r-lib.org/reference/slide_index.md)
  [`slide_index_chr()`](https://slider.r-lib.org/reference/slide_index.md)
  [`slide_index_dfr()`](https://slider.r-lib.org/reference/slide_index.md)
  [`slide_index_dfc()`](https://slider.r-lib.org/reference/slide_index.md)
  : Slide relative to an index
- [`slide_index2()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`slide_index2_vec()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`slide_index2_dbl()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`slide_index2_int()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`slide_index2_lgl()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`slide_index2_chr()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`slide_index2_dfr()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`slide_index2_dfc()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index_vec()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index_dbl()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index_int()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index_lgl()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index_chr()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index_dfr()`](https://slider.r-lib.org/reference/slide_index2.md)
  [`pslide_index_dfc()`](https://slider.r-lib.org/reference/slide_index2.md)
  : Slide along multiples inputs simultaneously relative to an index
- [`slide_index_sum()`](https://slider.r-lib.org/reference/summary-index.md)
  [`slide_index_prod()`](https://slider.r-lib.org/reference/summary-index.md)
  [`slide_index_mean()`](https://slider.r-lib.org/reference/summary-index.md)
  [`slide_index_min()`](https://slider.r-lib.org/reference/summary-index.md)
  [`slide_index_max()`](https://slider.r-lib.org/reference/summary-index.md)
  [`slide_index_all()`](https://slider.r-lib.org/reference/summary-index.md)
  [`slide_index_any()`](https://slider.r-lib.org/reference/summary-index.md)
  : Specialized sliding functions relative to an index

## Slide period family

These functions work by iterating over `.x` in “period blocks”, such as
one month blocks of data. The blocks are defined using a combination of
a secondary date-like index vector, `.i`, and a period to block by.

- [`slide_period()`](https://slider.r-lib.org/reference/slide_period.md)
  [`slide_period_vec()`](https://slider.r-lib.org/reference/slide_period.md)
  [`slide_period_dbl()`](https://slider.r-lib.org/reference/slide_period.md)
  [`slide_period_int()`](https://slider.r-lib.org/reference/slide_period.md)
  [`slide_period_lgl()`](https://slider.r-lib.org/reference/slide_period.md)
  [`slide_period_chr()`](https://slider.r-lib.org/reference/slide_period.md)
  [`slide_period_dfr()`](https://slider.r-lib.org/reference/slide_period.md)
  [`slide_period_dfc()`](https://slider.r-lib.org/reference/slide_period.md)
  : Slide relative to an index chunked by period
- [`slide_period2()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`slide_period2_vec()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`slide_period2_dbl()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`slide_period2_int()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`slide_period2_lgl()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`slide_period2_chr()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`slide_period2_dfr()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`slide_period2_dfc()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period_vec()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period_dbl()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period_int()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period_lgl()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period_chr()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period_dfr()`](https://slider.r-lib.org/reference/slide_period2.md)
  [`pslide_period_dfc()`](https://slider.r-lib.org/reference/slide_period2.md)
  : Slide along multiple inputs simultaneously relative to an index
  chunked by period

## Hop family

These functions are lower level versions of their
[`slide()`](https://slider.r-lib.org/reference/slide.md) equivalents.
They allow you to manually construct the boundaries to slide with, and
are useful if you need more flexibility than what
[`slide()`](https://slider.r-lib.org/reference/slide.md) provides.

- [`hop()`](https://slider.r-lib.org/reference/hop.md)
  [`hop_vec()`](https://slider.r-lib.org/reference/hop.md) : Hop
- [`hop2()`](https://slider.r-lib.org/reference/hop2.md)
  [`hop2_vec()`](https://slider.r-lib.org/reference/hop2.md)
  [`phop()`](https://slider.r-lib.org/reference/hop2.md)
  [`phop_vec()`](https://slider.r-lib.org/reference/hop2.md) : Hop along
  multiple inputs simultaneously
- [`hop_index()`](https://slider.r-lib.org/reference/hop_index.md)
  [`hop_index_vec()`](https://slider.r-lib.org/reference/hop_index.md) :
  Hop relative to an index
- [`hop_index2()`](https://slider.r-lib.org/reference/hop_index2.md)
  [`hop_index2_vec()`](https://slider.r-lib.org/reference/hop_index2.md)
  [`phop_index()`](https://slider.r-lib.org/reference/hop_index2.md)
  [`phop_index_vec()`](https://slider.r-lib.org/reference/hop_index2.md)
  : Hop along multiple inputs simultaneously relative to an index

## Block

[`block()`](https://slider.r-lib.org/reference/block.md) breaks `.x`
into its “period blocks”. The blocks are defined using a combination of
a secondary date-like index vector, `.i`, and a period to block by. The
return value is always a list, and the elements of the list are slices
of `.x`.

- [`block()`](https://slider.r-lib.org/reference/block.md) : Break a
  vector into blocks
