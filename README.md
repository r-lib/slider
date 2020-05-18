
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slider

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/DavisVaughan/slider/branch/master/graph/badge.svg)](https://codecov.io/gh/DavisVaughan/slider?branch=master)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R build
status](https://github.com/DavisVaughan/slider/workflows/R-CMD-check/badge.svg)](https://github.com/DavisVaughan/slider/actions)
<!-- badges: end -->

slider provides a family of general purpose “sliding window” functions.
The API is purposefully *very* similar to purrr. The goal of these
functions is usually to compute rolling averages, cumulative sums,
rolling regressions, or other “window” based computations.

There are 3 core functions in slider:

  - `slide()` iterates over your data like
    [`purrr::map()`](https://purrr.tidyverse.org/reference/map.html),
    but uses a sliding window to do so. It is type-stable, and always
    returns a result with the same size as its input.

  - `slide_index()` computes a rolling calculation *relative to an
    index*. If you have ever wanted to compute something like a “3 month
    rolling average” where the number of days in each month is
    irregular, you might like this function.

  - `slide_period()` is similar to `slide_index()` in that it slides
    relative to an index, but it first breaks the index up into “time
    blocks”, like 2 month blocks of time, and then it slides over `.x`
    using indices defined by those blocks.

Each of these core functions have the same variants as `purrr::map()`.
For example, `slide()` has `slide_dbl()`, `slide2()`, and `pslide()`,
along with the other combinations of these variants that you might
expect from having previously used purrr.

To learn more about these three functions, read the [introduction
vignette](https://davisvaughan.github.io/slider/articles/slider.html).

## Installation

Install the released version from [CRAN](https://CRAN.R-project.org)
with:

``` r
install.packages("slider")
```

Install the development version from [GitHub](https://github.com/) with:

``` r
remotes::install_github("DavisVaughan/slider")
```

## Examples

The [help page for
`slide()`](https://davisvaughan.github.io/slider/reference/slide.html)
has many examples, but here are a few:

``` r
library(slider)
```

The classic example would be to do a moving average. `slide()` handles
this with a combination of the `.before` and `.after` arguments, which
control the width of the window and the alignment.

``` r
# Moving average (Aligned right)
# "The current element + 2 elements before"
slide_dbl(1:5, ~mean(.x), .before = 2)
#> [1] 1.0 1.5 2.0 3.0 4.0

# Align left
# "The current element + 2 elements after"
slide_dbl(1:5, ~mean(.x), .after = 2)
#> [1] 2.0 3.0 4.0 4.5 5.0

# Center aligned
# "The current element + 1 element before + 1 element after"
slide_dbl(1:5, ~mean(.x), .before = 1, .after = 1)
#> [1] 1.5 2.0 3.0 4.0 4.5
```

With `Inf`, you can do a “cumulative slide” to compute cumulative
expressions. I think of this as saying “give me everything before the
current element.”

``` r
slide(1:4, ~.x, .before = Inf)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 1 2 3
#> 
#> [[4]]
#> [1] 1 2 3 4
```

With `.complete`, you can decide whether or not `.f` should be evaluated
on incomplete windows. In the following example, the requested window
size is 3, but the first two results are computed on windows of size 1
and 2 because partial results are allowed by default. When `.complete`
is set to `TRUE`, the first two results are not computed.

``` r
slide(1:4, ~.x, .before = 2)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 1 2 3
#> 
#> [[4]]
#> [1] 2 3 4

slide(1:4, ~.x, .before = 2, .complete = TRUE)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> NULL
#> 
#> [[3]]
#> [1] 1 2 3
#> 
#> [[4]]
#> [1] 2 3 4
```

## Data frames

Unlike `purrr::map()`, `slide()` iterates over data frames in a row wise
fashion. Interestingly this means the default of `slide()` becomes a
generic row wise iterator, with nice syntax for accessing data frame
columns.

There is a [vignette specifically about
this](https://davisvaughan.github.io/slider/articles/rowwise.html).

``` r
mini_cars <- cars[1:4,]

slide(mini_cars, ~.x)
#> [[1]]
#>   speed dist
#> 1     4    2
#> 
#> [[2]]
#>   speed dist
#> 1     4   10
#> 
#> [[3]]
#>   speed dist
#> 1     7    4
#> 
#> [[4]]
#>   speed dist
#> 1     7   22

slide_dbl(mini_cars, ~.x$speed + .x$dist)
#> [1]  6 14 11 29
```

This makes rolling regressions trivial\!

``` r
library(tibble)
set.seed(123)

df <- tibble(
  y = rnorm(100),
  x = rnorm(100)
)

# Window size of 20 rows
# The current row + 19 before
# (see slide_index() for how to do this relative to a date vector!)
df$regressions <- slide(df, ~lm(y ~ x, data = .x), .before = 19, .complete = TRUE)

df[15:25,]
#> # A tibble: 11 x 3
#>         y      x regressions
#>     <dbl>  <dbl> <list>     
#>  1 -0.556  0.519 <NULL>     
#>  2  1.79   0.301 <NULL>     
#>  3  0.498  0.106 <NULL>     
#>  4 -1.97  -0.641 <NULL>     
#>  5  0.701 -0.850 <NULL>     
#>  6 -0.473 -1.02  <lm>       
#>  7 -1.07   0.118 <lm>       
#>  8 -0.218 -0.947 <lm>       
#>  9 -1.03  -0.491 <lm>       
#> 10 -0.729 -0.256 <lm>       
#> 11 -0.625  1.84  <lm>
```

## Index sliding

In many business settings, the value you want to compute is tied to some
*index*, like a date vector. In these cases, you’ll probably want to
compute sliding windows relative to the index, and not using the fixed
window that `slide()` provides. You can use `slide_index()` to pass in
both `.x` and an index, `.i`, and the window will be calculated relative
to that index.

Here, when computing a “2 day window”, you probably don’t want
`"2019-08-16"` and `"2019-08-18"` to be grouped together. `slide()` has
no concept of an index, so when you specify a window size of 2, it will
group these two together. `slide_index()`, on the other hand, will do
the right thing.

``` r
x <- 1:3
i <- as.Date(c("2019-08-15", "2019-08-16", "2019-08-18"))

# slide() has no concept of an "index"
slide(x, ~.x, .before = 1)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 2 3

# "index aware"
slide_index(x, i, ~.x, .before = 1)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 3
```

Essentially what happens is that when we get to `"2019-08-18"`, it
“looks backwards” 1 day to set a window boundary at `"2019-08-17"`.
Since the date at position 2, `"2019-08-16"`, is before `"2019-08-17"`,
it is not included.

Powerfully, you can pass through any object to `.before` that computes a
value from `.i - .before`. This means that you could also have used a
lubridate period object (which gets even more interesting when you use
`weeks()` or `months()`):

``` r
slide_index(x, i, ~.x, .before = lubridate::days(1))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
#> [[3]]
#> [1] 3
```

## Period sliding

`slide_period()` is different from `slide_index()` in that it first
breaks the index into “time blocks” and then slides over `.x` relative
to those blocks. For example, in the monthly period slide below, `i` is
broken up into 4 time blocks of “the current block of monthly data, plus
one block before this one”. The locations of those blocks are the
locations that are used to slice `.x` with.

``` r
i <- as.Date(c(
  "2019-01-29", 
  "2019-01-30", 
  "2019-02-05", 
  "2019-04-01", 
  "2019-05-10"
))

slide_period(i, i, "month", ~.x, .before = 1)
#> [[1]]
#> [1] "2019-01-29" "2019-01-30"
#> 
#> [[2]]
#> [1] "2019-01-29" "2019-01-30" "2019-02-05"
#> 
#> [[3]]
#> [1] "2019-04-01"
#> 
#> [[4]]
#> [1] "2019-04-01" "2019-05-10"
```

One neat thing to notice is that `slide_period()` is aware of the
distance between elements of `.i` in the period you specify. The
practical implication of this is that in the above example, group 3 with
`2019-04-01` did *not* include `2019-02-05` in it, because it is more
than 1 month group away.

## Inspiration

This package is inspired heavily by SQL’s window functions. The API is
similar, but more general because you can iterate over any kind of R
object.

There have been multiple attempts at creating sliding window functions
(I personally created `rollify()`, and worked a little bit on
`tsibble::slide()` with [Earo Wang](https://github.com/earowang)).

  - `zoo::rollapply()`
  - `tibbletime::rollify()`
  - `tsibble::slide()`

I believe that slider is the next iteration of these. There are a few
reasons for this:

  - To me, the API is more intuitive, and is more flexible because
    `.before` and `.after` let you completely control the entry point
    (as opposed to fixed entry points like `"center"`, `"left"`, etc.

  - It is objectively faster because it is written purely in C.

  - With `slide_vec()` you can return any kind of object, and are not
    limited to the suffixed versions: `_dbl`, `_int`, etc.

  - It iterates rowwise over data frames, consistent with the vctrs
    framework.

  - I believe it is overall more consistent, backed by a theory that can
    always justify the sliding window generated by any combination of
    the parameters.

Earo and I have spoken, and we have mutually agreed that it would be
best to deprecate `tsibble::slide()` in favor of `slider::slide()`.

Additionally, [data.table](https://github.com/Rdatatable/data.table)’s
non-equi joins have been pretty much the only solution to the problem
that `slide_index()` tries to solve. Their solution is robust and quite
fast, and has been a nice benchmark for slider. slider is trying to
solve a much narrower problem, so the API here is more focused.

## Performance

In terms of performance, be aware that any specialized package that
shifts the function calls to C are going to be faster than slider. For
example, `RcppRoll::roll_mean()` computes the rolling mean *at the C
level*, which is bound to be faster. The purpose of slider is to be
*general purpose*, while still being as fast as possible. This means
that it can be used for more abstract things, like rolling regressions,
or any other custom function that you want to use in a rolling fashion.

Otherwise, like `purrr::map()`, `slide()` is optimized in C to be as
fast as possible, getting out of the way as quickly as it can so the
main overhead are the `.f` calls.

## References

I’ve found the following references very useful to understand more about
window functions:

  - [Postgres SQL
    documentation](https://www.postgresql.org/docs/9.1/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS)

  - [dbplyr window function
    vignette](https://dbplyr.tidyverse.org/articles/translation-function.html#window-functions)

  - [SQLite documentation - with a
    flowchart](https://www.sqlite.org/windowfunctions.html)

  - [Vertica Rows vs Range
    discussion](https://www.vertica.com/docs/9.2.x/HTML/Content/Authoring/SQLReferenceManual/Functions/Analytic/window_frame_clause.htm?origin_team=T02V9CHFH#ROWSversusRANGE)
