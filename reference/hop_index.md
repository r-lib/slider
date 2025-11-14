# Hop relative to an index

`hop_index()` is the lower level engine that powers
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md). It
has slightly different invariants than
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md),
and is useful when you either need to hand craft boundary values, or
want to compute a result with a size that is different from `.x`.

## Usage

``` r
hop_index(.x, .i, .starts, .stops, .f, ...)

hop_index_vec(.x, .i, .starts, .stops, .f, ..., .ptype = NULL)
```

## Arguments

- .x:

  `[vector]`

  The vector to iterate over and apply `.f` to.

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

## Value

A vector fulfilling the following invariants:

### `hop_index()`

- `vec_size(hop_index(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_ptype(hop_index(.x, .starts, .stops)) == list()`

### `hop_index_vec()`

- `vec_size(hop_index_vec(.x, .starts, .stops)) == vec_size_common(.starts, .stops)`

- `vec_size(hop_index_vec(.x, .starts, .stops)[[1]]) == 1L`

- `vec_ptype(hop_index_vec(.x, .starts, .stops, .ptype = ptype)) == ptype`

## See also

[`slide()`](https://slider.r-lib.org/reference/slide.md),
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md),
[`hop_index2()`](https://slider.r-lib.org/reference/hop_index2.md)

## Examples

``` r
library(vctrs)
library(lubridate, warn.conflicts = FALSE)

# ---------------------------------------------------------------------------
# Returning a size smaller than `.x`

i <- as.Date("2019-01-25") + c(0, 1, 2, 3, 10, 20, 35, 42, 45)

# slide_index() allows you to slide relative to `i`
slide_index(i, i, ~.x, .before = weeks(1))
#> [[1]]
#> [1] "2019-01-25"
#> 
#> [[2]]
#> [1] "2019-01-25" "2019-01-26"
#> 
#> [[3]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27"
#> 
#> [[4]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28"
#> 
#> [[5]]
#> [1] "2019-01-28" "2019-02-04"
#> 
#> [[6]]
#> [1] "2019-02-14"
#> 
#> [[7]]
#> [1] "2019-03-01"
#> 
#> [[8]]
#> [1] "2019-03-01" "2019-03-08"
#> 
#> [[9]]
#> [1] "2019-03-08" "2019-03-11"
#> 

# But you might be more interested in coarser summaries. This groups
# by year-month and computes 2 `.f` on 2 month windows.
i_yearmonth <- year(i) + (month(i) - 1) / 12
slide_index(i, i_yearmonth, ~.x, .before = 1)
#> [[1]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28"
#> 
#> [[2]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28"
#> 
#> [[3]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28"
#> 
#> [[4]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28"
#> 
#> [[5]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28" "2019-02-04"
#> [6] "2019-02-14"
#> 
#> [[6]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28" "2019-02-04"
#> [6] "2019-02-14"
#> 
#> [[7]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28" "2019-02-04"
#> [6] "2019-02-14" "2019-03-01" "2019-03-08" "2019-03-11"
#> 
#> [[8]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28" "2019-02-04"
#> [6] "2019-02-14" "2019-03-01" "2019-03-08" "2019-03-11"
#> 
#> [[9]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28" "2019-02-04"
#> [6] "2019-02-14" "2019-03-01" "2019-03-08" "2019-03-11"
#> 

# ^ This works nicely when working with dplyr if you are trying to create
# a new column in a data frame, but you'll notice that there are really only
# 3 months, so only 3 values are being calculated. If you only want to return
# a vector of those 3 values, you can use `hop_index()`. You'll have to
# hand craft the boundaries, but this is a general strategy
# I've found useful:
first_start <- floor_date(i[1], "months")
last_stop <- ceiling_date(i[length(i)], "months")
dates <- seq(first_start, last_stop, "1 month")
inner <- dates[2:(length(dates) - 1L)]
starts <- vec_c(first_start, inner)
stops <- vec_c(inner - 1, last_stop)

hop_index(i, i, starts, stops, ~.x)
#> [[1]]
#> [1] "2019-01-25" "2019-01-26" "2019-01-27" "2019-01-28"
#> 
#> [[2]]
#> [1] "2019-02-04" "2019-02-14"
#> 
#> [[3]]
#> [1] "2019-03-01" "2019-03-08" "2019-03-11"
#> 

# ---------------------------------------------------------------------------
# Non-existant dates with `lubridate::months()`

# Imagine you want to compute a 1 month rolling average on this
# irregular daily data.
i <- vec_c(as.Date("2019-02-27") + 0:3, as.Date("2019-03-27") + 0:5)
x <- rnorm(vec_seq_along(i))

# You might try `slide_index()` like this, but you'd run into this error
library(rlang)

with_options(
  catch_cnd(
    slide_index(x, i, mean, .before = months(1))
  ),
  rlang_backtrace_on_error = current_env()
)
#> <error/slider_error_generated_endpoints_cannot_be_na>
#> Error in `slide_index()`:
#> ℹ In locations: 7, 8, and 9
#> ! Endpoints generated by `.before` can't be `NA`.
#> ---
#> Backtrace:
#>      ▆
#>   1. └─pkgdown::build_site_github_pages(new_process = FALSE, install = FALSE)
#>   2.   └─pkgdown::build_site(...)
#>   3.     └─pkgdown:::build_site_local(...)
#>   4.       └─pkgdown::build_reference(...)
#>   5.         ├─pkgdown:::unwrap_purrr_error(...)
#>   6.         │ └─base::withCallingHandlers(...)
#>   7.         └─purrr::map(...)
#>   8.           └─purrr:::map_("list", .x, .f, ..., .progress = .progress)
#>   9.             ├─purrr:::with_indexed_errors(...)
#>  10.             │ └─base::withCallingHandlers(...)
#>  11.             ├─purrr:::call_with_cleanup(...)
#>  12.             └─pkgdown (local) .f(.x[[i]], ...)
#>  13.               ├─base::withCallingHandlers(...)
#>  14.               └─pkgdown:::data_reference_topic(...)
#>  15.                 └─pkgdown:::run_examples(...)
#>  16.                   └─pkgdown:::highlight_examples(code, topic, env = env)
#>  17.                     └─downlit::evaluate_and_highlight(...)
#>  18.                       └─evaluate::evaluate(code, child_env(env), new_device = TRUE, output_handler = output_handler)
#>  19.                         ├─base::withRestarts(...)
#>  20.                         │ └─base (local) withRestartList(expr, restarts)
#>  21.                         │   ├─base (local) withOneRestart(withRestartList(expr, restarts[-nr]), restarts[[nr]])
#>  22.                         │   │ └─base (local) doWithOneRestart(return(expr), restart)
#>  23.                         │   └─base (local) withRestartList(expr, restarts[-nr])
#>  24.                         │     └─base (local) withOneRestart(expr, restarts[[1L]])
#>  25.                         │       └─base (local) doWithOneRestart(return(expr), restart)
#>  26.                         ├─evaluate:::with_handlers(...)
#>  27.                         │ ├─base::eval(call)
#>  28.                         │ │ └─base::eval(call)
#>  29.                         │ └─base::withCallingHandlers(...)
#>  30.                         ├─base::withVisible(eval(expr, envir))
#>  31.                         └─base::eval(expr, envir)
#>  32.                           └─base::eval(expr, envir)
#>  33.                             ├─rlang::with_options(...)
#>  34.                             ├─rlang::catch_cnd(slide_index(x, i, mean, .before = months(1)))
#>  35.                             │ ├─rlang::eval_bare(...)
#>  36.                             │ ├─base::tryCatch(...)
#>  37.                             │ │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
#>  38.                             │ │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
#>  39.                             │ │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
#>  40.                             │ └─base::force(expr)
#>  41.                             └─slider::slide_index(x, i, mean, .before = months(1))

# This is because when you actually compute the `.i - .before` sequence,
# you hit non-existant dates. i.e. `"2019-03-29" - months(1)` doesn't exist.
i - months(1)
#>  [1] "2019-01-27" "2019-01-28" "2019-02-01" "2019-02-02" "2019-02-27"
#>  [6] "2019-02-28" NA           NA           NA           "2019-03-01"

# To get around this, lubridate provides `add_with_rollback()`,
# and the shortcut operation `%m-%`, which subtracts the month, then rolls
# forward/backward if it hits an `NA`. You can manually generate boundaries,
# then provide them to `hop_index()`.
starts <- i %m-% months(1)
stops <- i

hop_index(x, i, starts, stops, mean)
#> [[1]]
#> [1] 0.6215527
#> 
#> [[2]]
#> [1] 0.8849822
#> 
#> [[3]]
#> [1] -0.01728444
#> 
#> [[4]]
#> [1] -0.07479466
#> 
#> [[5]]
#> [1] -0.1086756
#> 
#> [[6]]
#> [1] -0.2895273
#> 
#> [[7]]
#> [1] -0.333556
#> 
#> [[8]]
#> [1] -0.1960505
#> 
#> [[9]]
#> [1] 0.08658389
#> 
#> [[10]]
#> [1] -0.2608412
#> 

hop_index(i, i, starts, stops, ~.x)
#> [[1]]
#> [1] "2019-02-27"
#> 
#> [[2]]
#> [1] "2019-02-27" "2019-02-28"
#> 
#> [[3]]
#> [1] "2019-02-27" "2019-02-28" "2019-03-01"
#> 
#> [[4]]
#> [1] "2019-02-27" "2019-02-28" "2019-03-01" "2019-03-02"
#> 
#> [[5]]
#> [1] "2019-02-27" "2019-02-28" "2019-03-01" "2019-03-02" "2019-03-27"
#> 
#> [[6]]
#> [1] "2019-02-28" "2019-03-01" "2019-03-02" "2019-03-27" "2019-03-28"
#> 
#> [[7]]
#> [1] "2019-02-28" "2019-03-01" "2019-03-02" "2019-03-27" "2019-03-28"
#> [6] "2019-03-29"
#> 
#> [[8]]
#> [1] "2019-02-28" "2019-03-01" "2019-03-02" "2019-03-27" "2019-03-28"
#> [6] "2019-03-29" "2019-03-30"
#> 
#> [[9]]
#> [1] "2019-02-28" "2019-03-01" "2019-03-02" "2019-03-27" "2019-03-28"
#> [6] "2019-03-29" "2019-03-30" "2019-03-31"
#> 
#> [[10]]
#> [1] "2019-03-01" "2019-03-02" "2019-03-27" "2019-03-28" "2019-03-29"
#> [6] "2019-03-30" "2019-03-31" "2019-04-01"
#> 
```
