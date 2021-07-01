# slider 0.2.2

* Updated internal usage of `vec_order()` to prepare for a breaking change
  in vctrs (#153).

# slider 0.2.1

* Fixed a C alignment issue detected by CRAN's USBAN machine related to
  allocating vectors of `long double`.

* Fixed a test that relied too strongly on the size of the C type,
  `long double`, which can vary across platforms (#147).

* Fixed an out of sync vignette entry (#148).

# slider 0.2.0

* New family of very fast specialized sliding functions:

  - `slide_sum()`, `slide_index_sum()`: for rolling sums
  
  - `slide_mean()`, `slide_index_mean()`: for rolling averages
  
  - `slide_prod()`, `slide_index_prod()`: for rolling products
  
  - `slide_min()`, `slide_index_min()`: for rolling minimums
  
  - `slide_max()`, `slide_index_max()`: for rolling maximums
  
  - `slide_any()`, `slide_index_any()`: for rolling any
  
  - `slide_all()`, `slide_index_all()`: for rolling all

* The `slide_index_*()` family now allows `.before` and `.after` to be
  functions of 1 argument (the index) that compute the boundaries of the
  sliding window. This can be extremely useful when the default, which computes
  `.i - .before` and `.i + .after`, is not applicable or correct for your needs.
  One use case is to set `.before = ~.x %m-% months(1)` rather than
  `.before = months(1)` to perform a 1 month rolling window in a way that won't
  generate `NA` values on invalid dates (like 1 month before 2019-03-31) (#139).

* The `slide_index_*()` family has undergone some internal changes to make it
  more compatible with custom vctrs classes that could be provided as the
  index (`.i`), such as the date-time classes in the clock package (#133, #130).
  
* For the `slide_index_*()` family, it is now required that `.i - .before` and
  `.i + .after` be castable to `.i` by `vctrs::vec_cast()`. Similarly, for
  the `hop_index_*()` family, `.starts` and `.stops` must both be castable to
  `.i` (#132).

* New vignette, `vignette("tsibble")`, explaining how to transition from tsibble
  to slider (#128).

* `vignette("rowwise")` has been updated to use `cur_data()` from dplyr 1.0.0,
  which makes it significantly easier to do rolling operations on data frames
  (like rolling regressions) using slider in a dplyr pipeline.

# slider 0.1.5

* `slide_period()` and friends have slightly better handling of size zero
  input when `.complete = TRUE` (#111).

* Better error messages for `NA` input with `.before`, `.after`, `.step` and
  `.complete` have been added (#110).

* A few instances of possibly unsafe C protection usage have been fixed (#112).

* Tests have been updated to use only numeric values in the `vctrs::new_date()`
  constructor (#113).

# slider 0.1.4

* As a followup to a change in slider 0.1.3, edge cases with size zero input
  in `hop()` have also been fixed.
  
* C code has been refactored to be less reliant on vctrs internals.

# slider 0.1.3

* Updated to stay compatible with vctrs 0.3.0.

* A few edge cases with size zero input in the index functions have been fixed.

* The default for the `.names_to` argument of `*_dfr()` variants has been
  updated to `rlang::zap()` to match the default of the function it is passed
  on to, `vctrs::vec_rbind()`.

* All `*_vec()` variants now maintain size stability when auto-simplifying
  (i.e. when `.ptype = NULL`) (#78, #93).

* `hop()` and its variants no longer place the names of `.x` on the output.
  Because there is no _size_ guarantee on the output, the size of `.x` can
  be different than the size of the output, meaning that the names might not
  line up. This also affects `slide_period()`, which is implemented using
  a `hop()` variant (#75).

* With data frames containing row names, `slide()` and its variants now copy
  those row names onto the output. This is an implicit benefit from vctrs
  gaining better support for data frame row names.

# slider 0.1.2

* Updated to stay compatible with the latest version of vctrs.

# slider 0.1.1

* Fixed a "multiple definition" C issue when compiling with gcc10.

# slider 0.1.0

* Added a `NEWS.md` file to track changes to the package.
