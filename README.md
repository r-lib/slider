
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slurrr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/DavisVaughan/slurrr.svg?branch=master)](https://travis-ci.org/DavisVaughan/slurrr)
<!-- badges: end -->

The goal of slurrr is to provide a general purpose sliding window
iterator, inspired heavily by SQL’s window functions.

For examples and documentation, see `?slide`.

## Installation

You can NOT install the released version of slurrr from
[CRAN](https://CRAN.R-project.org) yet.

And the development version from [GitHub](https://github.com/) with:

``` r
remotes::install_github("DavisVaughan/slurrr")
```

## Examples

The help page for `slide()` has many examples, but here are a few:

``` r
library(slurrr)
```

The classic example would be to do a moving average. `slide()` handles
this with a combination of the `.before` and `.after` arguments, which
control the width of the window and the alignment.

``` r
# Moving average (Aligned right)
slide_dbl(1:5, ~mean(.x), .before = 2)
#> [1] 1.0 1.5 2.0 3.0 4.0

# Align left
slide_dbl(1:5, ~mean(.x), .after = 2)
#> [1] 2.0 3.0 4.0 4.5 5.0

# Center aligned
slide_dbl(1:5, ~mean(.x), .before = 1, .after = 1)
#> [1] 1.5 2.0 3.0 4.0 4.5
```

With `unbounded()`, you can do a “cumulative slide” to compute
cumulative expressions.

``` r
slide(1:4, ~.x, .before = unbounded())
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

# De-cumulative (?) sliding
slide(1:4, ~.x, .after = unbounded())
#> [[1]]
#> [1] 1 2 3 4
#> 
#> [[2]]
#> [1] 2 3 4
#> 
#> [[3]]
#> [1] 3 4
#> 
#> [[4]]
#> [1] 4
```

With `.complete`, you can decide whether or not `.f` should be evaluated
on incomplete windows. In the following example, the requested window
size is 3, but the first two results are computed on windows of size 1
and 2 because partial results are allowed by default.

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

``` r
cars <- mtcars[1:4,]

slide(cars, ~.x)
#> [[1]]
#>           mpg cyl disp  hp drat   wt  qsec vs am gear carb
#> Mazda RX4  21   6  160 110  3.9 2.62 16.46  0  1    4    4
#> 
#> [[2]]
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> 
#> [[3]]
#>             mpg cyl disp hp drat   wt  qsec vs am gear carb
#> Datsun 710 22.8   4  108 93 3.85 2.32 18.61  1  1    4    1
#> 
#> [[4]]
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1

slide_dbl(cars, ~.x$mpg + .x$drat)
#> [1] 24.90 24.90 26.65 24.48
```

You can still use all of the other arguments to `slide()` to flexibly
slide over data frames too:

``` r
slide(cars, ~.x, .before = 2)
#> [[1]]
#>           mpg cyl disp  hp drat   wt  qsec vs am gear carb
#> Mazda RX4  21   6  160 110  3.9 2.62 16.46  0  1    4    4
#> 
#> [[2]]
#>               mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21   6  160 110  3.9 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21   6  160 110  3.9 2.875 17.02  0  1    4    4
#> 
#> [[3]]
#>                mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4     21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710    22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 
#> [[4]]
#>                 mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4 Wag  21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710     22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
```

## Window functions

A good explanation of window
functions

<https://www.postgresql.org/docs/9.1/sql-expressions.html#SYNTAX-WINDOW-FUNCTIONS>

With a
flowchart

<https://www.sqlite.org/windowfunctions.html>

dbplyr

<https://dbplyr.tidyverse.org/articles/translation-function.html#window-functions>

Rows vs Range (range = logical offset such as dates / offset from
current row’s integer
value)

<https://www.vertica.com/docs/9.2.x/HTML/Content/Authoring/SQLReferenceManual/Functions/Analytic/window_frame_clause.htm?origin_team=T02V9CHFH#ROWSversusRANGE>
