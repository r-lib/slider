#ifndef SLIDER_SUMMARY_MACROS
#define SLIDER_SUMMARY_MACROS

// -----------------------------------------------------------------------------
// Sum

#define SUM_IMPL_NA_KEEP                                       \
  double val = 0.0;                                            \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    val += p_x[j];                                             \
  }


#define SUM_IMPL_NA_RM                                         \
  double val = 0.0;                                            \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    const double elt = p_x[j];                                 \
                                                               \
    if (!isnan(elt)) {                                         \
      val += elt;                                              \
    }                                                          \
  }

// -----------------------------------------------------------------------------
// Mean

#define MEAN_IMPL_NA_KEEP                                      \
  long double val = 0.0;                                       \
  R_xlen_t window_size = window_stop - window_start;           \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    val += p_x[j];                                             \
  }                                                            \
  val /= window_size;                                          \
                                                               \
  /* No second pass required if known to be NA/NaN/Inf/-Inf */ \
  if (!R_FINITE((double) val)) {                               \
    p_out[i] = (double) val;                                   \
    continue;                                                  \
  }                                                            \
                                                               \
  long double adjustment = 0.0;                                \
                                                               \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {      \
    adjustment += (p_x[j] - val);                              \
  }                                                            \
  adjustment /= window_size;                                   \
                                                               \
  val += adjustment;


#define MEAN_IMPL_NA_RM                                          \
  long double val = 0.0;                                         \
  R_xlen_t window_size = 0;                                      \
                                                                 \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {        \
    double elt = p_x[j];                                         \
                                                                 \
    if (!isnan(elt)) {                                           \
      val += elt;                                                \
      ++window_size;                                             \
    }                                                            \
  }                                                              \
  val /= window_size;                                            \
                                                                 \
  /* No second pass required if known to be NA/NaN/Inf/-Inf */   \
  if (!R_FINITE((double) val)) {                                 \
    p_out[i] = (double) val;                                     \
    continue;                                                    \
  }                                                              \
                                                                 \
  long double adjustment = 0.0;                                  \
                                                                 \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {        \
    double elt = p_x[j];                                         \
                                                                 \
    if (!isnan(elt)) {                                           \
      adjustment += (elt - val);                                 \
    }                                                            \
  }                                                              \
  adjustment /= window_size;                                     \
                                                                 \
  val += adjustment;

// -----------------------------------------------------------------------------
// Prod

#define PROD_IMPL_NA_KEEP                                        \
  double val = 1.0;                                              \
                                                                 \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {        \
    val *= p_x[j];                                               \
  }


#define PROD_IMPL_NA_RM                                          \
  double val = 1.0;                                              \
                                                                 \
  for (R_xlen_t j = window_start; j < window_stop; ++j) {        \
    const double elt = p_x[j];                                   \
                                                                 \
    if (!isnan(elt)) {                                           \
      val *= elt;                                                \
    }                                                            \
}

// -----------------------------------------------------------------------------
#endif
