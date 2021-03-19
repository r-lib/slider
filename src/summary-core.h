#ifndef SLIDER_SUMMARY_CORE
#define SLIDER_SUMMARY_CORE

#include "slider.h"
#include "summary-core-types.h"
#include "align.h"

// From `summary-core-align.hpp`
size_t align_of_long_double();
size_t align_of_mean_state_t();

// -----------------------------------------------------------------------------
// Sum

static inline void sum_state_reset(void* p_state) {
  long double* p_state_ = (long double*) p_state;
  *p_state_ = 0;
}

static inline void sum_state_finalize(void* p_state, void* p_result) {
  double* p_result_ = (double*) p_result;
  long double state = *((long double*) p_state);

  if (state > DBL_MAX) {
    *p_result_ = R_PosInf;
  } else if (state < -DBL_MAX) {
    *p_result_ = R_NegInf;
  } else {
    *p_result_ = (double) state;
  }

  return;
}

static inline void* sum_nodes_increment(void* p_nodes) {
  return (void*) (((long double*) p_nodes) + 1);
}

static inline void* sum_nodes_void_deref(SEXP nodes) {
  return aligned_void_deref(nodes, align_of_long_double());
}
static inline long double* sum_nodes_deref(SEXP nodes) {
  return (long double*) sum_nodes_void_deref(nodes);
}

static inline SEXP sum_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(aligned_allocate(n, sizeof(long double), align_of_long_double()));
  long double* p_nodes = sum_nodes_deref(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i] = 0;
  }

  UNPROTECT(1);
  return nodes;
}

static inline void sum_na_keep_aggregate_from_leaves(const void* p_source,
                                                     uint64_t begin,
                                                     uint64_t end,
                                                     void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  // If already NaN or NA, nothing can change it
  // Huge performance increase here b/c of slow arithmetic with nan long doubles
  if (isnan(*p_dest_)) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (isnan(elt)) {
      *p_dest_ = elt;
      return;
    }

    *p_dest_ += elt;
  }
}

static inline void sum_na_keep_aggregate_from_nodes(const void* p_source,
                                                    uint64_t begin,
                                                    uint64_t end,
                                                    void* p_dest) {
  const long double* p_source_ = (const long double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  // If already NaN or NA, nothing can change it
  // Huge performance increase here b/c of slow arithmetic with nan long doubles
  if (isnan(*p_dest_)) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const long double elt = p_source_[i];

    if (isnan(elt)) {
      *p_dest_ = elt;
      return;
    }

    *p_dest_ += elt;
  }
}

static inline void sum_na_rm_aggregate_from_leaves(const void* p_source,
                                                   uint64_t begin,
                                                   uint64_t end,
                                                   void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (!isnan(elt)) {
      *p_dest_ += elt;
    }
  }
}

static inline void sum_na_rm_aggregate_from_nodes(const void* p_source,
                                                  uint64_t begin,
                                                  uint64_t end,
                                                  void* p_dest) {
  const long double* p_source_ = (const long double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const long double elt = p_source_[i];

    // Don't wrap with `if (!isnan(elt))`. Faster and more correct, this way
    // we propagate node `NaN` values resulting from `Inf + -Inf`
    *p_dest_ += elt;
  }
}

// -----------------------------------------------------------------------------
// Prod

static inline void prod_state_reset(void* p_state) {
  long double* p_state_ = (long double*) p_state;
  *p_state_ = 1;
}

static inline void prod_state_finalize(void* p_state, void* p_result) {
  double* p_result_ = (double*) p_result;
  long double state = *((long double*) p_state);

  if (state > DBL_MAX) {
    *p_result_ = R_PosInf;
  } else if (state < -DBL_MAX) {
    *p_result_ = R_NegInf;
  } else {
    *p_result_ = (double) state;
  }

  return;
}

static inline void* prod_nodes_increment(void* p_nodes) {
  return (void*) (((long double*) p_nodes) + 1);
}

static inline void* prod_nodes_void_deref(SEXP nodes) {
  return aligned_void_deref(nodes, align_of_long_double());
}
static inline long double* prod_nodes_deref(SEXP nodes) {
  return (long double*) prod_nodes_void_deref(nodes);
}

static inline SEXP prod_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(aligned_allocate(n, sizeof(long double), align_of_long_double()));
  long double* p_nodes = prod_nodes_deref(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i] = 1;
  }

  UNPROTECT(1);
  return nodes;
}

static inline void prod_na_keep_aggregate_from_leaves(const void* p_source,
                                                      uint64_t begin,
                                                      uint64_t end,
                                                      void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  // If already NaN or NA, nothing can change it
  // Huge performance increase here b/c of slow arithmetic with nan long doubles
  if (isnan(*p_dest_)) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (isnan(elt)) {
      *p_dest_ = elt;
      return;
    }

    *p_dest_ *= elt;
  }
}

static inline void prod_na_keep_aggregate_from_nodes(const void* p_source,
                                                     uint64_t begin,
                                                     uint64_t end,
                                                     void* p_dest) {
  const long double* p_source_ = (const long double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  // If already NaN or NA, nothing can change it
  // Huge performance increase here b/c of slow arithmetic with nan long doubles
  if (isnan(*p_dest_)) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const long double elt = p_source_[i];

    if (isnan(elt)) {
      *p_dest_ = elt;
      return;
    }

    *p_dest_ *= elt;
  }
}

static inline void prod_na_rm_aggregate_from_leaves(const void* p_source,
                                                    uint64_t begin,
                                                    uint64_t end,
                                                    void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (!isnan(elt)) {
      *p_dest_ *= elt;
    }
  }
}

static inline void prod_na_rm_aggregate_from_nodes(const void* p_source,
                                                   uint64_t begin,
                                                   uint64_t end,
                                                   void* p_dest) {
  const long double* p_source_ = (const long double*) p_source;
  long double* p_dest_ = (long double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const long double elt = p_source_[i];

    // Don't wrap with `if (!isnan(elt))`. Faster and more correct, this way
    // we propagate node `NaN` values resulting from `Inf + -Inf`
    *p_dest_ *= elt;
  }
}

// -----------------------------------------------------------------------------
// Mean

static inline void mean_state_reset(void* p_state) {
  struct mean_state_t* p_state_ = (struct mean_state_t*) p_state;
  p_state_->sum = 0;
  p_state_->count = 0;
}

static inline void mean_state_finalize(void* p_state, void* p_result) {
  struct mean_state_t* p_state_ = (struct mean_state_t*) p_state;
  double* p_result_ = (double*) p_result;
  *p_result_ = (double) (p_state_->sum / p_state_->count);
  return;
}

static inline void* mean_nodes_increment(void* p_nodes) {
  return (void*) (((struct mean_state_t*) p_nodes) + 1);
}

static inline void* mean_nodes_void_deref(SEXP nodes) {
  return aligned_void_deref(nodes, align_of_mean_state_t());
}
static inline struct mean_state_t* mean_nodes_deref(SEXP nodes) {
  return (struct mean_state_t*) mean_nodes_void_deref(nodes);
}

static inline SEXP mean_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(aligned_allocate(n, sizeof(struct mean_state_t), align_of_mean_state_t()));
  struct mean_state_t* p_nodes = mean_nodes_deref(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i].sum = 0;
    p_nodes[i].count = 0;
  }

  UNPROTECT(1);
  return nodes;
}

static inline void mean_na_keep_aggregate_from_leaves(const void* p_source,
                                                      uint64_t begin,
                                                      uint64_t end,
                                                      void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  struct mean_state_t* p_dest_ = (struct mean_state_t*) p_dest;

  // If already NaN or NA, nothing can change it
  // Huge performance increase here b/c of slow arithmetic with nan long doubles
  if (isnan(p_dest_->sum)) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (isnan(elt)) {
      // No need to worry about count
      p_dest_->sum = elt;
      return;
    }

    p_dest_->sum += elt;
    ++p_dest_->count;
  }
}

static inline void mean_na_keep_aggregate_from_nodes(const void* p_source,
                                                     uint64_t begin,
                                                     uint64_t end,
                                                     void* p_dest) {
  const struct mean_state_t* p_source_ = (const struct mean_state_t*) p_source;
  struct mean_state_t* p_dest_ = (struct mean_state_t*) p_dest;

  // If already NaN or NA, nothing can change it
  // Huge performance increase here b/c of slow arithmetic with nan long doubles
  if (isnan(p_dest_->sum)) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const long double sum = p_source_[i].sum;

    if (isnan(sum)) {
      // No need to worry about count
      p_dest_->sum = sum;
      return;
    }

    p_dest_->sum += sum;
    p_dest_->count += p_source_[i].count;
  }
}

static inline void mean_na_rm_aggregate_from_leaves(const void* p_source,
                                                    uint64_t begin,
                                                    uint64_t end,
                                                    void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  struct mean_state_t* p_dest_ = (struct mean_state_t*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (!isnan(elt)) {
      p_dest_->sum += elt;
      ++p_dest_->count;
    }
  }
}

static inline void mean_na_rm_aggregate_from_nodes(const void* p_source,
                                                   uint64_t begin,
                                                   uint64_t end,
                                                   void* p_dest) {
  const struct mean_state_t* p_source_ = (const struct mean_state_t*) p_source;
  struct mean_state_t* p_dest_ = (struct mean_state_t*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    // Don't wrap with `if (!isnan(source.sum))`. Faster and more correct,
    // this way we propagate node `NaN` values resulting from `Inf + -Inf`
    p_dest_->sum += p_source_[i].sum;
    p_dest_->count += p_source_[i].count;
  }
}

// -----------------------------------------------------------------------------
// Min

static inline void min_state_reset(void* p_state) {
  double* p_state_ = (double*) p_state;
  *p_state_ = R_PosInf;
}

static inline void min_state_finalize(void* p_state, void* p_result) {
  double* p_state_ = (double*) p_state;
  double* p_result_ = (double*) p_result;
  *p_result_ = *p_state_;
  return;
}

static inline void* min_nodes_increment(void* p_nodes) {
  return (void*) (((double*) p_nodes) + 1);
}

static inline double* min_nodes_deref(SEXP nodes) {
  return REAL(nodes);
}
static inline void* min_nodes_void_deref(SEXP nodes) {
  return (void*) min_nodes_deref(nodes);
}

static inline SEXP min_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(Rf_allocVector(REALSXP, n));
  double* p_nodes = min_nodes_deref(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i] = R_PosInf;
  }

  UNPROTECT(1);
  return nodes;
}

static inline void min_na_keep_aggregate_from_leaves(const void* p_source,
                                                     uint64_t begin,
                                                     uint64_t end,
                                                     void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  double* p_dest_ = (double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (isnan(elt)) {
      /* Match R - any `NA` trumps `NaN` */
      if (ISNA(elt)) {
        *p_dest_ = NA_REAL;
        break;
      } else {
        *p_dest_ = R_NaN;
      }
    } else if (elt < *p_dest_) {
      *p_dest_ = elt;
    }
  }
}

static inline void min_na_keep_aggregate_from_nodes(const void* p_source,
                                                    uint64_t begin,
                                                    uint64_t end,
                                                    void* p_dest) {
  min_na_keep_aggregate_from_leaves(p_source, begin, end, p_dest);
}

static inline void min_na_rm_aggregate_from_leaves(const void* p_source,
                                                   uint64_t begin,
                                                   uint64_t end,
                                                   void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  double* p_dest_ = (double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (elt < *p_dest_) {
      *p_dest_ = elt;
    }
  }
}

static inline void min_na_rm_aggregate_from_nodes(const void* p_source,
                                                  uint64_t begin,
                                                  uint64_t end,
                                                  void* p_dest) {
  min_na_rm_aggregate_from_leaves(p_source, begin, end, p_dest);
}

// -----------------------------------------------------------------------------
// Max

static inline void max_state_reset(void* p_state) {
  double* p_state_ = (double*) p_state;
  *p_state_ = R_NegInf;
}

static inline void max_state_finalize(void* p_state, void* p_result) {
  double* p_state_ = (double*) p_state;
  double* p_result_ = (double*) p_result;
  *p_result_ = *p_state_;
  return;
}

static inline void* max_nodes_increment(void* p_nodes) {
  return (void*) (((double*) p_nodes) + 1);
}

static inline double* max_nodes_deref(SEXP nodes) {
  return REAL(nodes);
}
static inline void* max_nodes_void_deref(SEXP nodes) {
  return (void*) max_nodes_deref(nodes);
}

static inline SEXP max_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(Rf_allocVector(REALSXP, n));
  double* p_nodes = max_nodes_deref(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i] = R_NegInf;
  }

  UNPROTECT(1);
  return nodes;
}

static inline void max_na_keep_aggregate_from_leaves(const void* p_source,
                                                     uint64_t begin,
                                                     uint64_t end,
                                                     void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  double* p_dest_ = (double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (isnan(elt)) {
      /* Match R - any `NA` trumps `NaN` */
      if (ISNA(elt)) {
        *p_dest_ = NA_REAL;
        break;
      } else {
        *p_dest_ = R_NaN;
      }
    } else if (elt > *p_dest_) {
      *p_dest_ = elt;
    }
  }
}

static inline void max_na_keep_aggregate_from_nodes(const void* p_source,
                                                    uint64_t begin,
                                                    uint64_t end,
                                                    void* p_dest) {
  max_na_keep_aggregate_from_leaves(p_source, begin, end, p_dest);
}

static inline void max_na_rm_aggregate_from_leaves(const void* p_source,
                                                   uint64_t begin,
                                                   uint64_t end,
                                                   void* p_dest) {
  const double* p_source_ = (const double*) p_source;
  double* p_dest_ = (double*) p_dest;

  for (uint64_t i = begin; i < end; ++i) {
    const double elt = p_source_[i];

    if (elt > *p_dest_) {
      *p_dest_ = elt;
    }
  }
}

static inline void max_na_rm_aggregate_from_nodes(const void* p_source,
                                                  uint64_t begin,
                                                  uint64_t end,
                                                  void* p_dest) {
  max_na_rm_aggregate_from_leaves(p_source, begin, end, p_dest);
}

// -----------------------------------------------------------------------------
// All

static inline void all_state_reset(void* p_state) {
  int* p_state_ = (int*) p_state;
  *p_state_ = 1;
}

static inline void all_state_finalize(void* p_state, void* p_result) {
  int* p_result_ = (int*) p_result;
  const int state = *((int*) p_state);
  *p_result_ = state;
  return;
}

static inline void* all_nodes_increment(void* p_nodes) {
  return (void*) (((int*) p_nodes) + 1);
}

static inline int* all_nodes_deref(SEXP nodes) {
  return LOGICAL(nodes);
}
static inline void* all_nodes_void_deref(SEXP nodes) {
  return (void*) all_nodes_deref(nodes);
}

static inline SEXP all_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_nodes = all_nodes_deref(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i] = 1;
  }

  UNPROTECT(1);
  return nodes;
}

static inline void all_na_keep_aggregate_from_leaves(const void* p_source,
                                                     uint64_t begin,
                                                     uint64_t end,
                                                     void* p_dest) {
  const int* p_source_ = (const int*) p_source;
  int* p_dest_ = (int*) p_dest;

  // If already FALSE, we are done.
  // FALSE-ness overrides any potential NAs.
  if (!*p_dest_) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const int elt = p_source_[i];

    if (!elt) {
      *p_dest_ = 0;
      return;
    }

    if (elt == NA_LOGICAL) {
      *p_dest_ = NA_LOGICAL;
      continue;
    }
  }
}

static inline void all_na_keep_aggregate_from_nodes(const void* p_source,
                                                    uint64_t begin,
                                                    uint64_t end,
                                                    void* p_dest) {
  all_na_keep_aggregate_from_leaves(p_source, begin, end, p_dest);
}

static inline void all_na_rm_aggregate_from_leaves(const void* p_source,
                                                   uint64_t begin,
                                                   uint64_t end,
                                                   void* p_dest) {
  const int* p_source_ = (const int*) p_source;
  int* p_dest_ = (int*) p_dest;

  // If already FALSE, we are done.
  if (!*p_dest_) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const int elt = p_source_[i];

    if (!elt) {
      *p_dest_ = 0;
      return;
    }
  }
}

static inline void all_na_rm_aggregate_from_nodes(const void* p_source,
                                                  uint64_t begin,
                                                  uint64_t end,
                                                  void* p_dest) {
  all_na_rm_aggregate_from_leaves(p_source, begin, end, p_dest);
}

// -----------------------------------------------------------------------------
// Any

static inline void any_state_reset(void* p_state) {
  int* p_state_ = (int*) p_state;
  *p_state_ = 0;
}

static inline void any_state_finalize(void* p_state, void* p_result) {
  int* p_result_ = (int*) p_result;
  const int state = *((int*) p_state);
  *p_result_ = state;
  return;
}

static inline void* any_nodes_increment(void* p_nodes) {
  return (void*) (((int*) p_nodes) + 1);
}

static inline int* any_nodes_deref(SEXP nodes) {
  return LOGICAL(nodes);
}
static inline void* any_nodes_void_deref(SEXP nodes) {
  return (void*) any_nodes_deref(nodes);
}

static inline SEXP any_nodes_initialize(uint64_t n) {
  SEXP nodes = PROTECT(Rf_allocVector(LGLSXP, n));
  int* p_nodes = any_nodes_deref(nodes);

  for (uint64_t i = 0; i < n; ++i) {
    p_nodes[i] = 0;
  }

  UNPROTECT(1);
  return nodes;
}

static inline void any_na_keep_aggregate_from_leaves(const void* p_source,
                                                     uint64_t begin,
                                                     uint64_t end,
                                                     void* p_dest) {
  const int* p_source_ = (const int*) p_source;
  int* p_dest_ = (int*) p_dest;

  // If already TRUE, we are done.
  // TRUE-ness overrides any potential NAs.
  if (*p_dest_ == 1) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const int elt = p_source_[i];

    if (!elt) {
      continue;
    }

    if (elt == NA_LOGICAL) {
      *p_dest_ = NA_LOGICAL;
      continue;
    }

    *p_dest_ = 1;
    return;
  }
}

static inline void any_na_keep_aggregate_from_nodes(const void* p_source,
                                                    uint64_t begin,
                                                    uint64_t end,
                                                    void* p_dest) {
  any_na_keep_aggregate_from_leaves(p_source, begin, end, p_dest);
}

static inline void any_na_rm_aggregate_from_leaves(const void* p_source,
                                                   uint64_t begin,
                                                   uint64_t end,
                                                   void* p_dest) {
  const int* p_source_ = (const int*) p_source;
  int* p_dest_ = (int*) p_dest;

  // If already TRUE, we are done.
  if (*p_dest_ == 1) {
    return;
  }

  for (uint64_t i = begin; i < end; ++i) {
    const int elt = p_source_[i];

    if (elt == 1) {
      *p_dest_ = 1;
      return;
    }
  }
}

static inline void any_na_rm_aggregate_from_nodes(const void* p_source,
                                                  uint64_t begin,
                                                  uint64_t end,
                                                  void* p_dest) {
  any_na_rm_aggregate_from_leaves(p_source, begin, end, p_dest);
}

// -----------------------------------------------------------------------------
#endif
