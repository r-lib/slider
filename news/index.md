# Changelog

## slider 0.3.3

- Fixed the C level function signature for a vctrs callable
  ([\#224](https://github.com/r-lib/slider/issues/224)).

- Removed usage of non-API `OBJECT()`.

## slider 0.3.2

CRAN release: 2024-10-25

- Removed usage of non-API `STRING_PTR()`
  ([\#209](https://github.com/r-lib/slider/issues/209)).

- R \>=4.0.0 is now required, inline with tidyverse guidelines.

- Updated snapshot tests to use the latest version of cli.

## slider 0.3.1

CRAN release: 2023-10-12

- Fixed tests that failed under valgrind due to `NA` vs `NaN`
  peculiarities ([\#198](https://github.com/r-lib/slider/issues/198)).

- Bumped required versions of vctrs, cli, and rlang to their current
  CRAN versions.

- Bumped required R version to \>= 3.6.0.

## slider 0.3.0

CRAN release: 2022-11-16

- Improved reported error calls and error messages throughout the
  package, and switched from glue to cli
  ([\#168](https://github.com/r-lib/slider/issues/168),
  [\#188](https://github.com/r-lib/slider/issues/188)).

- Index (`.i`) types that aren’t explicitly understood by vctrs are now
  handled slightly better
  ([\#182](https://github.com/r-lib/slider/issues/182)).

- The `slide_index_*()` and `hop_index_*()` families now use
  [`vctrs::vec_rank()`](https://vctrs.r-lib.org/reference/vec_rank.html)
  internally to compute a dense rank, which should be a little faster
  than the previous home grown approach
  ([\#177](https://github.com/r-lib/slider/issues/177)).

- New
  [`slider_plus()`](https://slider.r-lib.org/reference/index-arithmetic.md)
  and
  [`slider_minus()`](https://slider.r-lib.org/reference/index-arithmetic.md)
  developer facing helpers that allow package authors to register custom
  double dispatch methods to override the default computation of
  `.i - .before` and `.i + .after` when generating index bounds
  internally. This is intended to allow the clock and almanac packages
  to register methods so their custom types can be used natively in
  slider ([\#91](https://github.com/r-lib/slider/issues/91)).

- Removed ellipsis in favor of using the equivalent functions in rlang
  ([\#185](https://github.com/r-lib/slider/issues/185)).

- Removed `R_forceAndCall()` fallback now that R \>=3.4.0 is required
  ([\#172](https://github.com/r-lib/slider/issues/172)).

- Fixed `-Wstrict-prototypes` warnings as requested by CRAN
  ([\#173](https://github.com/r-lib/slider/issues/173)).

- Bumped minimal version of R to \>=3.4.0 to align with tidyverse
  standards.

- Bumped minimal version of rlang to \>=1.0.6 and vctrs to \>=0.5.0
  ([\#165](https://github.com/r-lib/slider/issues/165),
  [\#174](https://github.com/r-lib/slider/issues/174)).

- Fixed redirecting URLs.

## slider 0.2.2

CRAN release: 2021-07-01

- Updated internal usage of
  [`vec_order()`](https://vctrs.r-lib.org/reference/vec_order.html) to
  prepare for a breaking change in vctrs
  ([\#153](https://github.com/r-lib/slider/issues/153)).

## slider 0.2.1

CRAN release: 2021-03-23

- Fixed a C alignment issue detected by CRAN’s USBAN machine related to
  allocating vectors of `long double`.

- Fixed a test that relied too strongly on the size of the C type,
  `long double`, which can vary across platforms
  ([\#147](https://github.com/r-lib/slider/issues/147)).

- Fixed an out of sync vignette entry
  ([\#148](https://github.com/r-lib/slider/issues/148)).

## slider 0.2.0

CRAN release: 2021-03-18

- New family of very fast specialized sliding functions:

  - [`slide_sum()`](https://slider.r-lib.org/reference/summary-slide.md),
    [`slide_index_sum()`](https://slider.r-lib.org/reference/summary-index.md):
    for rolling sums

  - [`slide_mean()`](https://slider.r-lib.org/reference/summary-slide.md),
    [`slide_index_mean()`](https://slider.r-lib.org/reference/summary-index.md):
    for rolling averages

  - [`slide_prod()`](https://slider.r-lib.org/reference/summary-slide.md),
    [`slide_index_prod()`](https://slider.r-lib.org/reference/summary-index.md):
    for rolling products

  - [`slide_min()`](https://slider.r-lib.org/reference/summary-slide.md),
    [`slide_index_min()`](https://slider.r-lib.org/reference/summary-index.md):
    for rolling minimums

  - [`slide_max()`](https://slider.r-lib.org/reference/summary-slide.md),
    [`slide_index_max()`](https://slider.r-lib.org/reference/summary-index.md):
    for rolling maximums

  - [`slide_any()`](https://slider.r-lib.org/reference/summary-slide.md),
    [`slide_index_any()`](https://slider.r-lib.org/reference/summary-index.md):
    for rolling any

  - [`slide_all()`](https://slider.r-lib.org/reference/summary-slide.md),
    [`slide_index_all()`](https://slider.r-lib.org/reference/summary-index.md):
    for rolling all

- The `slide_index_*()` family now allows `.before` and `.after` to be
  functions of 1 argument (the index) that compute the boundaries of the
  sliding window. This can be extremely useful when the default, which
  computes `.i - .before` and `.i + .after`, is not applicable or
  correct for your needs. One use case is to set
  `.before = ~.x %m-% months(1)` rather than `.before = months(1)` to
  perform a 1 month rolling window in a way that won’t generate `NA`
  values on invalid dates (like 1 month before 2019-03-31)
  ([\#139](https://github.com/r-lib/slider/issues/139)).

- The `slide_index_*()` family has undergone some internal changes to
  make it more compatible with custom vctrs classes that could be
  provided as the index (`.i`), such as the date-time classes in the
  clock package ([\#133](https://github.com/r-lib/slider/issues/133),
  [\#130](https://github.com/r-lib/slider/issues/130)).

- For the `slide_index_*()` family, it is now required that
  `.i - .before` and `.i + .after` be castable to `.i` by
  [`vctrs::vec_cast()`](https://vctrs.r-lib.org/reference/vec_cast.html).
  Similarly, for the `hop_index_*()` family, `.starts` and `.stops` must
  both be castable to `.i`
  ([\#132](https://github.com/r-lib/slider/issues/132)).

- New vignette,
  [`vignette("tsibble")`](https://slider.r-lib.org/articles/tsibble.md),
  explaining how to transition from tsibble to slider
  ([\#128](https://github.com/r-lib/slider/issues/128)).

- [`vignette("rowwise")`](https://slider.r-lib.org/articles/rowwise.md)
  has been updated to use
  [`cur_data()`](https://dplyr.tidyverse.org/reference/deprec-context.html)
  from dplyr 1.0.0, which makes it significantly easier to do rolling
  operations on data frames (like rolling regressions) using slider in a
  dplyr pipeline.

## slider 0.1.5

CRAN release: 2020-07-21

- [`slide_period()`](https://slider.r-lib.org/reference/slide_period.md)
  and friends have slightly better handling of size zero input when
  `.complete = TRUE`
  ([\#111](https://github.com/r-lib/slider/issues/111)).

- Better error messages for `NA` input with `.before`, `.after`, `.step`
  and `.complete` have been added
  ([\#110](https://github.com/r-lib/slider/issues/110)).

- A few instances of possibly unsafe C protection usage have been fixed
  ([\#112](https://github.com/r-lib/slider/issues/112)).

- Tests have been updated to use only numeric values in the
  [`vctrs::new_date()`](https://vctrs.r-lib.org/reference/new_date.html)
  constructor ([\#113](https://github.com/r-lib/slider/issues/113)).

## slider 0.1.4

CRAN release: 2020-05-28

- As a followup to a change in slider 0.1.3, edge cases with size zero
  input in [`hop()`](https://slider.r-lib.org/reference/hop.md) have
  also been fixed.

- C code has been refactored to be less reliant on vctrs internals.

## slider 0.1.3

CRAN release: 2020-05-14

- Updated to stay compatible with vctrs 0.3.0.

- A few edge cases with size zero input in the index functions have been
  fixed.

- The default for the `.names_to` argument of `*_dfr()` variants has
  been updated to
  [`rlang::zap()`](https://rlang.r-lib.org/reference/zap.html) to match
  the default of the function it is passed on to,
  [`vctrs::vec_rbind()`](https://vctrs.r-lib.org/reference/vec_bind.html).

- All `*_vec()` variants now maintain size stability when
  auto-simplifying (i.e. when `.ptype = NULL`)
  ([\#78](https://github.com/r-lib/slider/issues/78),
  [\#93](https://github.com/r-lib/slider/issues/93)).

- [`hop()`](https://slider.r-lib.org/reference/hop.md) and its variants
  no longer place the names of `.x` on the output. Because there is no
  *size* guarantee on the output, the size of `.x` can be different than
  the size of the output, meaning that the names might not line up. This
  also affects
  [`slide_period()`](https://slider.r-lib.org/reference/slide_period.md),
  which is implemented using a
  [`hop()`](https://slider.r-lib.org/reference/hop.md) variant
  ([\#75](https://github.com/r-lib/slider/issues/75)).

- With data frames containing row names,
  [`slide()`](https://slider.r-lib.org/reference/slide.md) and its
  variants now copy those row names onto the output. This is an implicit
  benefit from vctrs gaining better support for data frame row names.

## slider 0.1.2

CRAN release: 2020-03-10

- Updated to stay compatible with the latest version of vctrs.

## slider 0.1.1

CRAN release: 2020-02-23

- Fixed a “multiple definition” C issue when compiling with gcc10.

## slider 0.1.0

CRAN release: 2020-02-06

- Added a `NEWS.md` file to track changes to the package.
