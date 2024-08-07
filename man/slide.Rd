% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slide.R
\name{slide}
\alias{slide}
\alias{slide_vec}
\alias{slide_dbl}
\alias{slide_int}
\alias{slide_lgl}
\alias{slide_chr}
\alias{slide_dfr}
\alias{slide_dfc}
\title{Slide}
\usage{
slide(.x, .f, ..., .before = 0L, .after = 0L, .step = 1L, .complete = FALSE)

slide_vec(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .ptype = NULL
)

slide_dbl(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_int(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_lgl(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_chr(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE
)

slide_dfr(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .names_to = rlang::zap(),
  .name_repair = c("unique", "universal", "check_unique")
)

slide_dfc(
  .x,
  .f,
  ...,
  .before = 0L,
  .after = 0L,
  .step = 1L,
  .complete = FALSE,
  .size = NULL,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
)
}
\arguments{
\item{.x}{\verb{[vector]}

The vector to iterate over and apply \code{.f} to.}

\item{.f}{\verb{[function / formula]}

If a \strong{function}, it is used as is.

If a \strong{formula}, e.g. \code{~ .x + 2}, it is converted to a function. There
are three ways to refer to the arguments:
\itemize{
\item For a single argument function, use \code{.}
\item For a two argument function, use \code{.x} and \code{.y}
\item For more arguments, use \code{..1}, \code{..2}, \code{..3} etc
}

This syntax allows you to create very compact anonymous functions.}

\item{...}{Additional arguments passed on to the mapped function.}

\item{.before, .after}{\verb{[integer(1) / Inf]}

The number of values before or after the current element to
include in the sliding window. Set to \code{Inf} to select all elements
before or after the current element. Negative values are allowed, which
allows you to "look forward" from the current element if used as the
\code{.before} value, or "look backwards" if used as \code{.after}.}

\item{.step}{\verb{[positive integer(1)]}

The number of elements to shift the window forward between function calls.}

\item{.complete}{\verb{[logical(1)]}

Should the function be evaluated on complete windows only? If \code{FALSE},
the default, then partial computations will be allowed.}

\item{.ptype}{\verb{[vector(0) / NULL]}

A prototype corresponding to the type of the output.

If \code{NULL}, the default, the output type is determined by computing the
common type across the results of the calls to \code{.f}.

If supplied, the result of each call to \code{.f} will be cast to that type,
and the final output will have that type.

If \code{getOption("vctrs.no_guessing")} is \code{TRUE}, the \code{.ptype} must be
supplied. This is a way to make production code demand fixed types.}

\item{.names_to}{This controls what to do with input names supplied in \code{...}.
\itemize{
\item By default, input names are \link[rlang:zap]{zapped}.
\item If a string, specifies a column where the input names will be
copied. These names are often useful to identify rows with
their original input. If a column name is supplied and \code{...} is
not named, an integer column is used instead.
\item If \code{NULL}, the input names are used as row names.
}}

\item{.name_repair}{One of \code{"unique"}, \code{"universal"}, \code{"check_unique"},
\code{"unique_quiet"}, or  \code{"universal_quiet"}. See \code{\link[vctrs:vec_as_names]{vec_as_names()}} for the
meaning of these options.

With \code{vec_rbind()}, the repair function is applied to all inputs
separately. This is because \code{vec_rbind()} needs to align their
columns before binding the rows, and thus needs all inputs to
have unique names. On the other hand, \code{vec_cbind()} applies the
repair function after all inputs have been concatenated together
in a final data frame. Hence \code{vec_cbind()} allows the more
permissive minimal names repair.}

\item{.size}{If, \code{NULL}, the default, will determine the number of rows in
\code{vec_cbind()} output by using the tidyverse \link[vctrs:theory-faq-recycling]{recycling rules}.

Alternatively, specify the desired number of rows, and any inputs of length
1 will be recycled appropriately.}
}
\value{
A vector fulfilling the following invariants:

\subsection{\code{slide()}}{
\itemize{
\item \code{vec_size(slide(.x)) == vec_size(.x)}
\item \code{vec_ptype(slide(.x)) == list()}
}

}

\subsection{\code{slide_vec()} and \verb{slide_*()} variants}{
\itemize{
\item \code{vec_size(slide_vec(.x)) == vec_size(.x)}
\item \code{vec_size(slide_vec(.x)[[1]]) == 1L}
\item \code{vec_ptype(slide_vec(.x, .ptype = ptype)) == ptype}
}

}
}
\description{
\code{slide()} iterates through \code{.x} using a sliding window, applying \code{.f} to each
sub-window of \code{.x}.
}
\details{
Unlike \code{lapply()} or \code{purrr::map()}, which construct calls like

\if{html}{\out{<div class="sourceCode">}}\preformatted{.f(.x[[i]], ...)
}\if{html}{\out{</div>}}

the equivalent with \code{slide()} looks like

\if{html}{\out{<div class="sourceCode">}}\preformatted{.f(vctrs::vec_slice(.x, i), ...)
}\if{html}{\out{</div>}}

which is approximately

\if{html}{\out{<div class="sourceCode">}}\preformatted{.f(.x[i], ...)
}\if{html}{\out{</div>}}

except in the case of data frames or arrays, which are iterated
over row-wise.

If \code{.x} has names, then the output will preserve those names.

Using \code{\link[vctrs:vec_cast]{vctrs::vec_cast()}}, the output of \code{.f} will be automatically cast
to the type required by the variant of \verb{slide_*()} being used.
}
\examples{
# The defaults work similarly to `map()`
slide(1:5, ~.x)

# Use `.before`, `.after`, and `.step` to control the window
slide(1:5, ~.x, .before = 1)

# This can be used for rolling means
slide_dbl(rnorm(5), mean, .before = 2)

# Or more flexible rolling operations
slide(rnorm(5), ~ .x - mean(.x), .before = 2)

# `.after` allows you to "align to the left" rather than the right
slide(1:5, ~.x, .after = 2)

# And a mixture of `.before` and `.after`
# allows you complete control over the exact alignment.
# Below, "center alignment" is used.
slide(1:5, ~.x, .before = 1, .after = 1)

# The `.step` controls how the window is shifted along `.x`,
# allowing you to "skip" iterations if you only need a less granular result
slide(1:10, ~.x, .before = 2, .step = 3)

# `.complete` controls whether or not partial results are computed.
# By default, they are, but setting `.complete = TRUE` restricts
# `slide()` to only evaluate the function where a complete window exists.
slide(1:5, ~.x, .before = 2, .after = 1)
slide(1:5, ~.x, .before = 2, .after = 1, .complete = TRUE)

# ---------------------------------------------------------------------------
# Data frames

# Data frames are iterated over rowwise
mtcars_rowwise <- slide(mtcars, ~.x)
mtcars_rowwise[1:3]

# This means that any column name is easily accessible
slide_dbl(mtcars, ~.x$mpg + .x$cyl)

# More advanced rowwise iteration is available as well by using the
# other arguments
mtcars_rowwise_window <- slide(mtcars, ~.x, .before = 1, .after = 1)
mtcars_rowwise_window[1:3]

# ---------------------------------------------------------------------------
# Cumulative sliding

# Using the special cased value, `Inf`, you can ask `slide()` to pin the
# start of the sliding window to the first element, effectively creating
# a cumulative window
slide(1:5, ~.x, .before = Inf)

# Same with `.after`, this creates a window where you start with all of the
# elements, but decrease the total number over each iteration
slide(1:5, ~.x, .after = Inf)

# ---------------------------------------------------------------------------
# Negative `.before` / `.after`

# `.before` is allowed to be negative, allowing you to "look forward" in
# your vector. Note that `abs(.before) <= .after` must hold if `.before` is
# negative. In this example, we look forward to elements in locations 2 and 3
# but place the result in position 1 in the output.
slide(1:5, ~.x, .before = -1, .after = 2)

# `.after` can be negative as well to "look backwards"
slide(1:5, ~.x, .before = 2, .after = -1)

# ---------------------------------------------------------------------------
# Removing padding

# If you are looking for a way to remove the `NA` values from something like
# this, then it doesn't exist as a built in option.
x <- rnorm(10)
slide_dbl(x, mean, .before = 3, .step = 2, .complete = TRUE)

# Adding an option to `slide_dbl()` to remove the `NA` values would destroy
# its size stability. Instead, you can use a combination of `slide_dfr()`
# to get the start/stop indices with `hop_index_vec()`.
i <- seq_along(x)
idx <- slide_dfr(
  i,
  ~data.frame(start = .x[1], stop = .x[length(.x)]),
  .before = 3,
  .step = 2,
  .complete = TRUE
)

idx

hop_index_vec(x, i, idx$start, idx$stop, mean, .ptype = double())

}
\seealso{
\code{\link[=slide2]{slide2()}}, \code{\link[=slide_index]{slide_index()}}, \code{\link[=hop]{hop()}}
}
