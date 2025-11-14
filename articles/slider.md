# Getting started with slider

``` r
library(slider)
library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
```

This vignette is meant to serve as an introduction to {slider}. In it,
you’ll learn about the three core functions in the package:
[`slide()`](https://slider.r-lib.org/reference/slide.md),
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md),
and
[`slide_period()`](https://slider.r-lib.org/reference/slide_period.md),
along with their many variants.

slider is a package for rolling analysis using window functions. “Window
functions” is a term that I’ve borrowed from SQL that means that some
function is repeatedly applied to different “windows” of your data as
you step through it. Typical examples of applications of window
functions include rolling averages, cumulative sums, and more complex
things such as rolling regressions.

## slide()

To better understand window functions, we’ll turn to our first core
function, [`slide()`](https://slider.r-lib.org/reference/slide.md).
[`slide()`](https://slider.r-lib.org/reference/slide.md) is a bit like
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html). You
supply a vector to slide over, `.x`, and a function to apply to each
window, `.f`. With those two things alone,
[`slide()`](https://slider.r-lib.org/reference/slide.md) is almost
identical to `map()`.

``` r
slide(1:4, ~.x)
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 3
#> 
#> [[4]]
#> [1] 4
```

On top of this, you can control the size and placement of the window by
using the additional arguments to
[`slide()`](https://slider.r-lib.org/reference/slide.md). For example,
you can ask for a window of size 3 containing “the current element, as
well as the 2 before it” like this:

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
```

You’ll notice that the first two elements of the list contain partial or
“incomplete” windows. By default,
[`slide()`](https://slider.r-lib.org/reference/slide.md) assumes that
you want to compute on these windows anyways, but if you don’t care
about them, you can change the `.complete` argument.

``` r
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

[`slide()`](https://slider.r-lib.org/reference/slide.md) is *size
stable*, so you always get an output that is the same size as your
input. Because of that, the partial results have been replaced by the
corresponding missing value. For a list, that is `NULL`.

Sometimes, changing the placement of the window is a critical part of
your calculation. For example, you might want a “center alignment” where
you have an equal number of values before and after the current element.
To accomplish this, you can combine the `.before` argument with `.after`
to get a centered window. Here we ask for a window of size 3 containing
“the current element, as well as 1 element before and 1 element after”.
It is “centered” because in position 2 we have a complete window of the
current element (2), along with one element before (1) and one after
(3).

``` r
slide(1:4, ~.x, .before = 1, .after = 1)
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1 2 3
#> 
#> [[3]]
#> [1] 2 3 4
#> 
#> [[4]]
#> [1] 3 4
```

[`slide()`](https://slider.r-lib.org/reference/slide.md) can also
perform *expanding* windows. These are the type that allow *cumulative*
operations to work. In prose, an expanding window would be “the current
element, along with every element before this one”. To construct this
kind of window, you can set `.before` to `Inf`.

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

[`slide()`](https://slider.r-lib.org/reference/slide.md) is
*type-stable*, meaning that it always returns an object of the same
type, and the base form of
[`slide()`](https://slider.r-lib.org/reference/slide.md) always returns
a list. So far, this is all that we have used to illustrate how it
works, but practically you are more likely to use one of the suffixed
forms like [`slide_dbl()`](https://slider.r-lib.org/reference/slide.md)
or [`slide_int()`](https://slider.r-lib.org/reference/slide.md). For
example, you might have a vector of sales data that you want to compute
a 3 value moving average on.

``` r
sales_vec <- c(2, 4, 6, 2)

slide_dbl(sales_vec, mean, .before = 2)
#> [1] 2 3 4 4
```

## slide_index()

To make things a bit more interesting, let’s assume that the sales
vector from the example above is also tied to some “index”, like a date
vector of when the sale actually occurred.

``` r
index_vec <- as.Date("2019-08-29") + c(0, 1, 5, 6)
wday_vec <- as.character(wday(index_vec, label = TRUE))

company <- tibble(
  sales = sales_vec,
  index = index_vec,
  wday = wday_vec
)

company
#> # A tibble: 4 × 3
#>   sales index      wday 
#>   <dbl> <date>     <chr>
#> 1     2 2019-08-29 Thu  
#> 2     4 2019-08-30 Fri  
#> 3     6 2019-09-03 Tue  
#> 4     2 2019-09-04 Wed
```

This index is increasing but irregular, meaning that we “jumped” from
Friday to Tuesday because there were no sales between those dates. For
the purpose of this example, let’s assume that this is an online company
where it is perfectly reasonable that you *could* have sales on both
Saturday and Sunday (If your use case requires that you “skip over”
weekends and even holidays, you might like
[{almanac}](https://github.com/DavisVaughan/almanac)).

A reasonable business question to ask would be to compute a *3 day*
moving average. Is this different from the 3 value moving average we
computed before? Here is the expected result, side by side with the 3
value one computed using
[`slide_dbl()`](https://slider.r-lib.org/reference/slide.md) from
before.

    #> # A tibble: 4 × 5
    #>   sales index      wday  roll_val roll_day
    #>   <dbl> <date>     <chr>    <dbl>    <dbl>
    #> 1     2 2019-08-29 Thu          2        2
    #> 2     4 2019-08-30 Fri          3        3
    #> 3     6 2019-09-03 Tue          4        6
    #> 4     2 2019-09-04 Wed          4        4

The difference shows up in the third row, when computing the 3 day
moving average looking back from Tuesday. To understand why they are
different, consider what
[`slide_dbl()`](https://slider.r-lib.org/reference/slide.md) does. It
uses the `sales` column and looks at the “current row, along with two
rows before it” to compute the result. When you are on row 3, this would
select rows 1-3 giving the date range of `[Thu, Tue]`, which isn’t 3
days. The correct answer would have been to look back 2 days from
Tuesday, not 2 rows from row 3. This would have given us the date window
of `[Sun, Tue]`, and only values in that range should be included in the
moving average calculation for row 3. The only row in that range is row
3, so we should just be averaging the single value of `6` to get our
result.

[`slide_dbl()`](https://slider.r-lib.org/reference/slide.md) doesn’t
give us what we want because it is *unaware of the index column*. It
just looks back a set number of values. What we need is a function that
“knows” about the `index` and can adjust accordingly. For that, you can
use `slide_index(.x, .i, .f, ...)` which has a `.i` argument to pass an
index vector through.

To understand how
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md)
works, take a look at the following comparison to
[`slide()`](https://slider.r-lib.org/reference/slide.md). For
illustration, the current window of the weekday vector is printed out.
Notice that in position 3,
[`slide()`](https://slider.r-lib.org/reference/slide.md) gives us the
“wrong” result of Thursday, Friday and Tuesday, because it just looks
back 2 values.

``` r
wday_vec
#> [1] "Thu" "Fri" "Tue" "Wed"

slide(wday_vec, ~.x, .before = 2)
#> [[1]]
#> [1] "Thu"
#> 
#> [[2]]
#> [1] "Thu" "Fri"
#> 
#> [[3]]
#> [1] "Thu" "Fri" "Tue"
#> 
#> [[4]]
#> [1] "Fri" "Tue" "Wed"
```

On the other hand,
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md) can
be “aware” of the irregular index vector. By passing it through as `.i`,
and by swapping a look back period of 2 for the lubridate object of
`days(2)`, the start of the range is computed as `.i - days(2)`, which
correctly computes a date window of `[Sun, Tue]` for the third element,
so that we only capture Tuesday in the window.

``` r
slide_index(wday_vec, index_vec, ~.x, .before = days(2))
#> [[1]]
#> [1] "Thu"
#> 
#> [[2]]
#> [1] "Thu" "Fri"
#> 
#> [[3]]
#> [1] "Tue"
#> 
#> [[4]]
#> [1] "Tue" "Wed"
```

Knowing this, we can swap out
[`slide_dbl()`](https://slider.r-lib.org/reference/slide.md) for
[`slide_index_dbl()`](https://slider.r-lib.org/reference/slide_index.md)
to see how to correctly compute our 3 day rolling average.

``` r
mutate(
  company, 
  roll_val = slide_dbl(sales, mean, .before = 2),
  roll_day = slide_index_dbl(sales, index, mean, .before = days(2))
)
#> # A tibble: 4 × 5
#>   sales index      wday  roll_val roll_day
#>   <dbl> <date>     <chr>    <dbl>    <dbl>
#> 1     2 2019-08-29 Thu          2        2
#> 2     4 2019-08-30 Fri          3        3
#> 3     6 2019-09-03 Tue          4        6
#> 4     2 2019-09-04 Wed          4        4
```

## slide_period()

With
[`slide_index()`](https://slider.r-lib.org/reference/slide_index.md), we
always returned a vector of the same size as `.x`, and the idea was to
build indices to slice `.x` with using “the current element of `.i` +
some number of elements before/after it”.
[`slide_period()`](https://slider.r-lib.org/reference/slide_period.md)
works a bit differently. It first breaks `.i` up into “time blocks” by
some period (like monthly), and then uses those blocks to define how to
slide over `.x`.

To see an example, let’s expand out our `company` sales data frame.

``` r
big_index_vec <- c(
  as.Date("2019-08-30") + 0:4,
  as.Date("2019-11-30") + 0:4
)

big_sales_vec <- c(2, 4, 6, 2, 8, 10, 9, 3, 5, 2)

big_company <- tibble(
  sales = big_sales_vec,
  index = big_index_vec
)

big_company
#> # A tibble: 10 × 2
#>    sales index     
#>    <dbl> <date>    
#>  1     2 2019-08-30
#>  2     4 2019-08-31
#>  3     6 2019-09-01
#>  4     2 2019-09-02
#>  5     8 2019-09-03
#>  6    10 2019-11-30
#>  7     9 2019-12-01
#>  8     3 2019-12-02
#>  9     5 2019-12-03
#> 10     2 2019-12-04
```

Now say we want to compute the monthly sales, and just return 1 value
per month. Since we have 4 months, we should get 4 values back. What we
really want to do here is break the `index` up into “time blocks” of 1
month, and then slide over those. That’s what
[`slide_period()`](https://slider.r-lib.org/reference/slide_period.md)
does.

``` r
slide_period(big_company, big_company$index, "month", ~.x)
#> [[1]]
#> # A tibble: 2 × 2
#>   sales index     
#>   <dbl> <date>    
#> 1     2 2019-08-30
#> 2     4 2019-08-31
#> 
#> [[2]]
#> # A tibble: 3 × 2
#>   sales index     
#>   <dbl> <date>    
#> 1     6 2019-09-01
#> 2     2 2019-09-02
#> 3     8 2019-09-03
#> 
#> [[3]]
#> # A tibble: 1 × 2
#>   sales index     
#>   <dbl> <date>    
#> 1    10 2019-11-30
#> 
#> [[4]]
#> # A tibble: 4 × 2
#>   sales index     
#>   <dbl> <date>    
#> 1     9 2019-12-01
#> 2     3 2019-12-02
#> 3     5 2019-12-03
#> 4     2 2019-12-04
```

Since this returns 4 values, and not the same number of values as there
are in `.x`, it won’t fit naturally in a
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) or
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
statement. I find the easiest way to do this is to create a helper
function that takes a data frame and returns one with the summary result
for one time block, and then call that with
[`slide_period_dfr()`](https://slider.r-lib.org/reference/slide_period.md).

``` r
monthly_summary <- function(data) {
  summarise(data, index = max(index), sales = sum(sales))
}

slide_period_dfr(
  big_company,
  big_company$index,
  "month",
  monthly_summary
)
#> # A tibble: 4 × 2
#>   index      sales
#>   <date>     <dbl>
#> 1 2019-08-31     6
#> 2 2019-09-03    16
#> 3 2019-11-30    10
#> 4 2019-12-04    19
```

Now you might be thinking, “I can do that with dplyr and lubridate!”,
and you’d be right:

``` r
big_company %>%
  mutate(monthly = floor_date(index, "month")) %>%
  group_by(monthly) %>%
  summarise(sales = sum(sales))
#> # A tibble: 4 × 2
#>   monthly    sales
#>   <date>     <dbl>
#> 1 2019-08-01     6
#> 2 2019-09-01    16
#> 3 2019-11-01    10
#> 4 2019-12-01    19
```

But here is where things get interesting! Now what if we want to compute
those monthly sales, but we want the time blocks to be made of the
“current month block, plus 1 month block before it”. For example, for
the month of `2019-09`, it would include `2019-08` and `2019-09`
together in the rolling summary. There isn’t an easy way to do this in
dplyr alone. With slider, there are two ways to do this.

The first is with
[`slide_period_dfr()`](https://slider.r-lib.org/reference/slide_period.md),
and it is as easy as adding `.before = 1`, to select the current month
block and 1 before it.

``` r
slide_period_dfr(
  big_company,
  big_company$index,
  "month",
  monthly_summary,
  .before = 1
)
#> # A tibble: 4 × 2
#>   index      sales
#>   <date>     <dbl>
#> 1 2019-08-31     6
#> 2 2019-09-03    22
#> 3 2019-11-30    10
#> 4 2019-12-04    29
```

Depending on your use case, you might want to append these results as a
new column in `big_company`. To do this, we can instead go back to using
[`floor_date()`](https://lubridate.tidyverse.org/reference/round_date.html)
to generate monthly groupings, and slide over them using
[`slide_index_dbl()`](https://slider.r-lib.org/reference/slide_index.md)
with a lookback period of 1 month.

``` r
big_company %>%
  mutate(
    monthly = floor_date(index, "month"),
    sales_summary = slide_index_dbl(sales, monthly, sum, .before = months(1))
  )
#> # A tibble: 10 × 4
#>    sales index      monthly    sales_summary
#>    <dbl> <date>     <date>             <dbl>
#>  1     2 2019-08-30 2019-08-01             6
#>  2     4 2019-08-31 2019-08-01             6
#>  3     6 2019-09-01 2019-09-01            22
#>  4     2 2019-09-02 2019-09-01            22
#>  5     8 2019-09-03 2019-09-01            22
#>  6    10 2019-11-30 2019-11-01            10
#>  7     9 2019-12-01 2019-12-01            29
#>  8     3 2019-12-02 2019-12-01            29
#>  9     5 2019-12-03 2019-12-01            29
#> 10     2 2019-12-04 2019-12-01            29
```
