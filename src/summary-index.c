#include "slider.h"
#include "slider-vctrs.h"
#include "opts-slide.h"
#include "utils.h"
#include "index.h"
#include "summary-macros.h"

// -----------------------------------------------------------------------------

typedef SEXP (*summary_index_fn)(SEXP x, SEXP i, SEXP starts, SEXP stops, SEXP indices, bool complete, bool na_rm);
typedef void (*summary_index_impl_fn)(const double* p_x,
                                      int iter_min,
                                      int iter_max,
                                      const struct range_info range,
                                      const int* window_sizes,
                                      const int* window_starts,
                                      const int* window_stops,
                                      SEXP indices,
                                      struct index_info* p_index,
                                      double* p_out);

static SEXP slider_index_summary(SEXP x,
                                 SEXP i,
                                 SEXP starts,
                                 SEXP stops,
                                 SEXP indices,
                                 SEXP complete,
                                 SEXP na_rm,
                                 summary_index_fn fn) {
  bool dot = false;
  bool c_complete = validate_complete(complete, dot);
  bool c_na_rm = validate_na_rm(na_rm, dot);
  return fn(x, i, starts, stops, indices, c_complete, c_na_rm);
}

static SEXP slide_index_summary(SEXP x,
                                SEXP i,
                                SEXP starts,
                                SEXP stops,
                                SEXP indices,
                                bool complete,
                                bool na_rm,
                                summary_index_impl_fn fn_na_keep,
                                summary_index_impl_fn fn_na_rm) {
  int n_prot = 0;

  // Before `vec_cast()`, which may drop names
  SEXP names = PROTECT_N(slider_names(x, SLIDE), &n_prot);

  x = PROTECT_N(vec_cast(x, slider_shared_empty_dbl), &n_prot);
  const double* p_x = REAL(x);

  const R_xlen_t size = Rf_xlength(x);

  SEXP out = PROTECT_N(slider_init(REALSXP, size), &n_prot);
  double* p_out = REAL(out);
  Rf_setAttrib(out, R_NamesSymbol, names);

  struct index_info index = new_index_info(i);
  PROTECT_INDEX_INFO(&index, &n_prot);

  int* window_sizes = (int*) R_alloc(index.size, sizeof(int));
  int* window_starts = (int*) R_alloc(index.size, sizeof(int));
  int* window_stops = (int*) R_alloc(index.size, sizeof(int));

  fill_window_info(window_sizes, window_starts, window_stops, indices, index.size);

  struct range_info range = new_range_info(starts, stops, index.size);
  PROTECT_RANGE_INFO(&range, &n_prot);

  const int iter_min = compute_min_iteration(index, range, complete);
  const int iter_max = compute_max_iteration(index, range, complete);

  if (na_rm) {
    fn_na_rm(p_x, iter_min, iter_max, range, window_sizes, window_starts, window_stops, indices, &index, p_out);
  } else {
    fn_na_keep(p_x, iter_min, iter_max, range, window_sizes, window_starts, window_stops, indices, &index, p_out);
  }

  UNPROTECT(n_prot);
  return out;
}

// -----------------------------------------------------------------------------

#define SLIDE_INDEX_SUMMARY_LOOP(IMPL) do {                       \
  for (int i = iter_min; i < iter_max; ++i) {                     \
    if (i % 1024 == 0) {                                          \
      R_CheckUserInterrupt();                                     \
    }                                                             \
                                                                  \
    int starts_pos = locate_window_starts_pos(p_index, range, i); \
    int stops_pos = locate_window_stops_pos(p_index, range, i);   \
                                                                  \
    int window_start = window_starts[starts_pos];                 \
    int window_stop = window_stops[stops_pos] + 1;                \
                                                                  \
    IMPL                                                          \
                                                                  \
    SEXP locations = VECTOR_ELT(indices, i);                      \
    const int* p_locations = INTEGER(locations);                  \
    const R_len_t size_locations = window_sizes[i];               \
                                                                  \
    for (R_len_t j = 0; j < size_locations; ++j) {                \
      R_len_t loc = p_locations[j] - 1;                           \
      p_out[loc] = val;                                           \
    }                                                             \
  }                                                               \
} while (0)

// -----------------------------------------------------------------------------

static SEXP slide_index_sum_impl(SEXP x, SEXP i, SEXP starts, SEXP stops, SEXP indices, bool complete, bool na_rm);

// [[ register() ]]
SEXP slider_index_sum_impl(SEXP x, SEXP i, SEXP starts, SEXP stops, SEXP indices, SEXP complete, SEXP na_rm) {
  return slider_index_summary(x, i, starts, stops, indices, complete, na_rm, slide_index_sum_impl);
}

static void slide_index_sum_impl_na_keep(const double* p_x,
                                         int iter_min,
                                         int iter_max,
                                         const struct range_info range,
                                         const int* window_sizes,
                                         const int* window_starts,
                                         const int* window_stops,
                                         SEXP indices,
                                         struct index_info* p_index,
                                         double* p_out);

static void slide_index_sum_impl_na_rm(const double* p_x,
                                       int iter_min,
                                       int iter_max,
                                       const struct range_info range,
                                       const int* window_sizes,
                                       const int* window_starts,
                                       const int* window_stops,
                                       SEXP indices,
                                       struct index_info* p_index,
                                       double* p_out);

static SEXP slide_index_sum_impl(SEXP x, SEXP i, SEXP starts, SEXP stops, SEXP indices, bool complete, bool na_rm) {
  return slide_index_summary(x, i, starts, stops, indices, complete, na_rm, slide_index_sum_impl_na_keep, slide_index_sum_impl_na_rm);
}

static void slide_index_sum_impl_na_keep(const double* p_x,
                                         int iter_min,
                                         int iter_max,
                                         const struct range_info range,
                                         const int* window_sizes,
                                         const int* window_starts,
                                         const int* window_stops,
                                         SEXP indices,
                                         struct index_info* p_index,
                                         double* p_out) {
  SLIDE_INDEX_SUMMARY_LOOP(SUM_IMPL_NA_KEEP);
}

static void slide_index_sum_impl_na_rm(const double* p_x,
                                       int iter_min,
                                       int iter_max,
                                       const struct range_info range,
                                       const int* window_sizes,
                                       const int* window_starts,
                                       const int* window_stops,
                                       SEXP indices,
                                       struct index_info* p_index,
                                       double* p_out) {
  SLIDE_INDEX_SUMMARY_LOOP(SUM_IMPL_NA_RM);
}

// -----------------------------------------------------------------------------

#undef SLIDE_INDEX_SUMMARY_LOOP
