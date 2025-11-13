# Row-wise iteration with slider

``` r
library(slider)
library(dplyr, warn.conflicts = FALSE)
```

slider is implemented with a new convention that began in vctrs,
treating a data frame as a vector of rows. This makes
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) a *row-wise
iterator* over a data frame, which can be useful for solving some
previously tricky problems in the tidyverse.

The point of this vignette is to go through a few examples of a
row-oriented workflow. The examples are adapted from [Jenny Bryan’s talk
of row-oriented workflows with
purrr](https://github.com/jennybc/row-oriented-workflows), to show how
this workflow is improved with
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md).

## Row-wise iteration

Let’s first explore using
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) as a row
wise iterator in general. We’ll start with this simple data frame.

``` r
example <- tibble(
  x = 1:4,
  y = letters[1:4]
)

example
#> # A tibble: 4 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     2 b    
#> 3     3 c    
#> 4     4 d
```

If we were to pass the `x` column to
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md), it would
iterate over that using the window specified by `.before`, `.after`, and
`.complete`. The defaults are similar to
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html).

``` r
slide(example$x, ~.x)
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

slide(example$x, ~.x, .before = 2)
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

When applied to the entire `example` data frame, `map()` treats it as a
list and iterates over the columns.
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md), on the
other hand, iterates over rows. This is consistent with the vctrs idea
of *size*, which is the length of an atomic vector, but the number of
rows of a data frame or matrix.
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) always
returns an object with the same *size* as its input. Because the number
of rows in `example` is 4, the output size is 4 and you get one row per
element in the output.

``` r
slide(example, ~.x)
#> [[1]]
#> # A tibble: 1 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 
#> [[2]]
#> # A tibble: 1 × 2
#>       x y    
#>   <int> <chr>
#> 1     2 b    
#> 
#> [[3]]
#> # A tibble: 1 × 2
#>       x y    
#>   <int> <chr>
#> 1     3 c    
#> 
#> [[4]]
#> # A tibble: 1 × 2
#>       x y    
#>   <int> <chr>
#> 1     4 d
```

You can still use the other arguments to
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) to control
the window size.

``` r
# Current row + 2 before
slide(example, ~.x, .before = 2)
#> [[1]]
#> # A tibble: 1 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 
#> [[2]]
#> # A tibble: 2 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     2 b    
#> 
#> [[3]]
#> # A tibble: 3 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     2 b    
#> 3     3 c    
#> 
#> [[4]]
#> # A tibble: 3 × 2
#>       x y    
#>   <int> <chr>
#> 1     2 b    
#> 2     3 c    
#> 3     4 d

# Center aligned, with no partial results
slide(example, ~.x, .before = 1, .after = 1, .complete = TRUE)
#> [[1]]
#> NULL
#> 
#> [[2]]
#> # A tibble: 3 × 2
#>       x y    
#>   <int> <chr>
#> 1     1 a    
#> 2     2 b    
#> 3     3 c    
#> 
#> [[3]]
#> # A tibble: 3 × 2
#>       x y    
#>   <int> <chr>
#> 1     2 b    
#> 2     3 c    
#> 3     4 d    
#> 
#> [[4]]
#> NULL
```

Often, using
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) with its
defaults will be enough, as it is common to iterate over just one row at
a time.

## Varying parameter combinations

A nice use of a tibble is as a structured way to store parameter
combinations. For example, we could store multiple rows of parameter
combinations where each row could be supplied to
[`runif()`](https://rdrr.io/r/stats/Uniform.html) to generate different
types of uniform random variables.

``` r
parameters <- tibble(
  n = 1:3,
  min = c(0, 10, 100),
  max = c(1, 100, 1000)
)

parameters
#> # A tibble: 3 × 3
#>       n   min   max
#>   <int> <dbl> <dbl>
#> 1     1     0     1
#> 2     2    10   100
#> 3     3   100  1000
```

With [`slide()`](https://slider.r-lib.org/dev/reference/slide.md) you
can pass these parameters on to
[`runif()`](https://rdrr.io/r/stats/Uniform.html) by iterating over
`parameters` row-wise. This gives you access to the data frame of the
current row through `.x`. Because it is a data frame, you have access to
each column by name. Notice how there is no restriction that the columns
of the data frame be the same as the argument names of
[`runif()`](https://rdrr.io/r/stats/Uniform.html).

``` r
set.seed(123)

slide(parameters, ~runif(.x$n, .x$min, .x$max))
#> [[1]]
#> [1] 0.2875775
#> 
#> [[2]]
#> [1] 80.94746 46.80792
#> 
#> [[3]]
#> [1] 894.7157 946.4206 141.0008
```

This can also be done with
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html), but
you either have to name the `parameters` tibble with the same column
names as the function you are calling, or you have to access each column
positionally as `..1`, `..3`, etc.

A third alternative that works nicely here is to use
[`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html) before
calling [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
Just remember to wrap the result of
[`runif()`](https://rdrr.io/r/stats/Uniform.html) in a
[`list()`](https://rdrr.io/r/base/list.html)!

``` r
parameters %>%
  rowwise() %>%
  mutate(random = list(runif(n, min, max)))
#> # A tibble: 3 × 4
#> # Rowwise: 
#>       n   min   max random   
#>   <int> <dbl> <dbl> <list>   
#> 1     1     0     1 <dbl [1]>
#> 2     2    10   100 <dbl [2]>
#> 3     3   100  1000 <dbl [3]>
```

## Sliding inside a mutate()

For these examples, we will consider a `company` data set containing the
`day` a sale was made, the number of calls, `n_calls`, that were placed
on that day, and the number of `sales` that resulted from those calls.

``` r
company <- tibble(
  day = rep(c(1, 2), each = 5),
  sales = sample(100, 10),
  n_calls = sales + sample(1000, 10)
)

company
#> # A tibble: 10 × 3
#>      day sales n_calls
#>    <dbl> <int>   <int>
#>  1     1    25     544
#>  2     1    90     516
#>  3     1    91     740
#>  4     1    69     835
#>  5     1    98     309
#>  6     2    57     989
#>  7     2    92     682
#>  8     2     9     602
#>  9     2    72     627
#> 10     2    26     897
```

When [`slide()`](https://slider.r-lib.org/dev/reference/slide.md)-ing
inside of a
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) call,
there are a few scenarios that can arise. First, you might want to slide
over a single column. This is easy enough in both the un-grouped and
grouped case.

``` r
company %>%
  mutate(sales_roll = slide_dbl(sales, mean, .before = 2, .complete = TRUE))
#> # A tibble: 10 × 4
#>      day sales n_calls sales_roll
#>    <dbl> <int>   <int>      <dbl>
#>  1     1    25     544       NA  
#>  2     1    90     516       NA  
#>  3     1    91     740       68.7
#>  4     1    69     835       83.3
#>  5     1    98     309       86  
#>  6     2    57     989       74.7
#>  7     2    92     682       82.3
#>  8     2     9     602       52.7
#>  9     2    72     627       57.7
#> 10     2    26     897       35.7

company %>%
  group_by(day) %>%
  mutate(sales_roll = slide_dbl(sales, mean, .before = 2, .complete = TRUE))
#> # A tibble: 10 × 4
#> # Groups:   day [2]
#>      day sales n_calls sales_roll
#>    <dbl> <int>   <int>      <dbl>
#>  1     1    25     544       NA  
#>  2     1    90     516       NA  
#>  3     1    91     740       68.7
#>  4     1    69     835       83.3
#>  5     1    98     309       86  
#>  6     2    57     989       NA  
#>  7     2    92     682       NA  
#>  8     2     9     602       52.7
#>  9     2    72     627       57.7
#> 10     2    26     897       35.7
```

If you need to apply a sliding function that takes a data frame as input
to slide over, then you’ll need some way to access the “current” data
frame that
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) is
acting on. As of dplyr 1.0.0, you can access this with
[`cur_data()`](https://dplyr.tidyverse.org/reference/deprec-context.html).
When there is only 1 group, the current data frame is the input itself,
but when there are multiple groups
[`cur_data()`](https://dplyr.tidyverse.org/reference/deprec-context.html)
returns the data frame corresponding to the current group that is being
worked on.

As an example, imagine you want to fit a rolling linear model predicting
sales from the number of calls. The most robust way to do this in a
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) is to
use
[`cur_data()`](https://dplyr.tidyverse.org/reference/deprec-context.html)
to access the data frame to slide over. Since
[`slide()`](https://slider.r-lib.org/dev/reference/slide.md) iterates
row-wise, `.x` corresponds to the current slice of the current data
frame.

``` r
company %>%
  mutate(
    regressions = slide(
      .x = cur_data(),
      .f = ~lm(sales ~ n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )
#> Warning: There was 1 warning in `mutate()`.
#> ℹ In argument: `regressions = slide(...)`.
#> Caused by warning:
#> ! `cur_data()` was deprecated in dplyr 1.1.0.
#> ℹ Please use `pick()` instead.
#> # A tibble: 10 × 4
#>      day sales n_calls regressions
#>    <dbl> <int>   <int> <list>     
#>  1     1    25     544 <NULL>     
#>  2     1    90     516 <NULL>     
#>  3     1    91     740 <lm>       
#>  4     1    69     835 <lm>       
#>  5     1    98     309 <lm>       
#>  6     2    57     989 <lm>       
#>  7     2    92     682 <lm>       
#>  8     2     9     602 <lm>       
#>  9     2    72     627 <lm>       
#> 10     2    26     897 <lm>
```

When you group by `day`,
[`cur_data()`](https://dplyr.tidyverse.org/reference/deprec-context.html)
will first correspond to all rows where `day == 1`, and then where
`day == 2`. Notice how the output has two clumps of `NULL`s, proving
that the rolling regressions “restarted” between groups.

``` r
company %>%
  group_by(day) %>%
  mutate(
    regressions = slide(
      .x = cur_data(),
      .f = ~lm(sales ~ n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )
#> # A tibble: 10 × 4
#> # Groups:   day [2]
#>      day sales n_calls regressions
#>    <dbl> <int>   <int> <list>     
#>  1     1    25     544 <NULL>     
#>  2     1    90     516 <NULL>     
#>  3     1    91     740 <lm>       
#>  4     1    69     835 <lm>       
#>  5     1    98     309 <lm>       
#>  6     2    57     989 <NULL>     
#>  7     2    92     682 <NULL>     
#>  8     2     9     602 <lm>       
#>  9     2    72     627 <lm>       
#> 10     2    26     897 <lm>
```

In the past, you might have used `.` in place of
[`cur_data()`](https://dplyr.tidyverse.org/reference/deprec-context.html).
This `.` is actually from the magrittr `%>%`, not from dplyr, and has a
few issues. The biggest one is that it won’t work with grouped data
frames, it will always return the entire data set rather than the
current group’s data frame. The other issue is that, even with
un-grouped data frames, you can’t take advantage of the sequential
nature of how
[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)
evaluates expressions. For example, the following doesn’t work because
`.` corresponds to `company` without the updated `log_sales` column.

``` r
company %>%
  mutate(
    log_sales = log10(sales),
    regressions = slide(
      .x = .,
      .f = ~lm(log_sales ~ n_calls, .x), 
      .before = 2, 
      .complete = TRUE
    )
  )
#> Error in `mutate()`:
#> ℹ In argument: `regressions = slide(...)`.
#> Caused by error in `model.frame.default()`:
#> ! variable lengths differ (found for 'n_calls')
```
