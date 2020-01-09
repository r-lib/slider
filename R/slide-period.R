slide_period <- function(.x,
                         .i,
                         .period,
                         .f,
                         ...,
                         .every = 1L,
                         .origin = NULL,
                         .before = 0L,
                         .after = 0L,
                         .complete = FALSE) {
  slide_period_impl(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = FALSE,
    .ptype = list()
  )
}

slide_period_vec <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE,
                             .ptype = list()) {

  if (is.null(.ptype)) {
    out <- slide_period_simplify(
      .x,
      .i,
      .period,
      .f,
      ...,
      .every = .every,
      .origin = .origin,
      .before = .before,
      .after = .after,
      .complete = .complete
    )

    return(out)
  }

  slide_period_impl(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .constrain = TRUE,
    .ptype = .ptype
  )
}

slide_period_simplify <- function(.x,
                                  .i,
                                  .period,
                                  .f,
                                  ...,
                                  .every,
                                  .origin,
                                  .before,
                                  .after,
                                  .complete) {
  out <- slide_period(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete
  )

  check_all_size_one(out)

  vec_c(!!!out)
}

slide_period_dbl <- function(.x,
                             .i,
                             .period,
                             .f,
                             ...,
                             .every = 1L,
                             .origin = NULL,
                             .before = 0L,
                             .after = 0L,
                             .complete = FALSE) {
  slide_period_vec(
    .x,
    .i,
    .period,
    .f,
    ...,
    .every = .every,
    .origin = .origin,
    .before = .before,
    .after = .after,
    .complete = .complete,
    .ptype = double()
  )
}

# ------------------------------------------------------------------------------

slide_period_impl <- function(.x,
                              .i,
                              .period,
                              .f,
                              ...,
                              .every,
                              .origin,
                              .before,
                              .after,
                              .complete,
                              .constrain,
                              .ptype) {
  vec_assert(.x)

  .f <- as_function(.f)

  f_call <- expr(.f(.x, ...))

  type <- -1L

  slide_period_common(
    x = .x,
    i = .i,
    period = .period,
    f_call = f_call,
    every = .every,
    origin = .origin,
    before = .before,
    after = .after,
    complete = .complete,
    constrain = .constrain,
    ptype = .ptype,
    env = environment(),
    type = type
  )
}

# ------------------------------------------------------------------------------

slide_period_common <- function(x,
                                i,
                                period,
                                f_call,
                                every,
                                origin,
                                before,
                                after,
                                complete,
                                constrain,
                                ptype,
                                env,
                                type) {
  check_block_index_type(i)

  before <- check_slide_period_before(before)
  after <- check_slide_period_after(after)
  complete <- check_slide_period_complete(complete)

  groups <- warp_distance(
    i,
    period = period,
    every = every,
    origin = origin
  )

  unique <- unique(groups)

  starts <- unique - before
  stops <- unique + after

  if (complete) {
    n <- length(unique)
    first <- unique[[1]]
    last <- unique[[n]]

    # TODO Eventually rewrite in C
    from <- compute_from(starts, first, n, before)
    to <- compute_to(stops, last, n, after)

    # Only slice if we have to
    # could use compact_seq() at C level to slice here
    # would have to check for `from>to->int()`
    if (from != 1L || to != n) {
      starts <- starts[rlang::seq2(from, to)]
      stops <- stops[rlang::seq2(from, to)]
    }
  }

  out <- hop_index_common(
    x = x,
    i = groups,
    starts = starts,
    stops = stops,
    f_call = f_call,
    constrain = constrain,
    ptype = ptype,
    env = env,
    type = type
  )

  if (!complete) {
    return(out)
  }

  # Pad with ptype
  ptype <- list()
  init <- vec_init(ptype, n = 1L)

  front <- vec_recycle(init, from - 1L)
  back <- vec_recycle(init, n - to)

  out <- vec_c(front, out, back)

  out
}

compute_from <- function(starts, first, n, before) {
  from <- 1L

  if (is.infinite(before)) {
    return(from)
  }

  for (i in seq_len(n)) {
    if (first > starts[[i]]) {
      from <- from + 1L
    } else {
      break
    }
  }

  from
}

compute_to <- function(stops, last, n, after) {
  to <- n

  if (is.infinite(after)) {
    return(to)
  }

  for (i in rev(seq_len(n))) {
    if (last < stops[[i]]) {
      to <- to - 1L
    } else {
      break
    }
  }

  to
}

check_slide_period_before <- function(x) {
  vec_assert(x, size = 1L, arg = ".before")

  if (is.infinite(x) && x > 0) {
    return(x)
  }

  x <- vec_cast(x, integer(), x_arg = ".before")

  if (is.na(x)) {
    abort("`.before` cannot be `NA`.")
  }

  x
}

check_slide_period_after <- function(x) {
  vec_assert(x, size = 1L, arg = ".after")

  if (is.infinite(x) && x > 0) {
    return(x)
  }

  x <- vec_cast(x, integer(), x_arg = ".after")

  if (is.na(x)) {
    abort("`.after` cannot be `NA`.")
  }

  x
}

check_slide_period_complete <- function(x) {
  vec_assert(x, size = 1L, arg = ".complete")

  x <- vec_cast(x, logical(), x_arg = ".complete")

  if (is.na(x)) {
    abort("`.complete` cannot be `NA`.")
  }

  x
}
