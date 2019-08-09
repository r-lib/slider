#include "slurrr.h"
#include "utils.h"
#include <vctrs.h>

// -----------------------------------------------------------------------------
// All defined below

static SEXP slice_container(int type);

static void update_slices(SEXP* p_slices, SEXP x, SEXP index, SEXP env, int type);

static SEXP copy_names(SEXP out, SEXP x, int type);

static int iterations(int x_start, int x_end, const struct slide_params params);

// -----------------------------------------------------------------------------

static SEXP slide(SEXP x,
                  SEXP f_call,
                  SEXP ptype,
                  SEXP env,
                  struct slide_params p) {

  // Bail if inputs are size 0
  if (p.size == 0) {
    return vec_init(ptype, 0);
  }

  int x_start;
  int x_end;
  int window_start;
  int window_end;
  int entry_step;
  int entry_offset;

  if (p.forward) {
    x_start = 0;
    x_end = p.size - 1;
    window_start = x_start - p.before + p.offset;
    window_end = window_start + (p.before + p.after);
    entry_step = p.step;
    entry_offset = p.offset;
  }
  else {
    x_start = p.size - 1;
    x_end = 0;
    window_start = x_start + p.after - p.offset;
    window_end = window_start - (p.before + p.after);
    entry_step = -p.step;
    entry_offset = -p.offset;
  }

  // `entry` has to be 1-based for `vec_assign_impl()`
  SEXP entry = PROTECT(r_int(x_start + entry_offset + 1));
  int* p_entry = INTEGER(entry);

  int n = iterations(x_start, x_end, p);

  int window_start_step = entry_step;
  int window_end_step = entry_step;
  if (p.forward) {
    if (p.before_unbounded) {
      window_start_step = 0;
    }
    if (p.after_unbounded) {
      window_end_step = 0;
    }
  } else {
    if (p.before_unbounded) {
      window_end_step = 0;
    }
    if (p.after_unbounded) {
      window_start_step = 0;
    }
  }

  // Init and proxy our `out` container
  PROTECT_INDEX out_prot_idx;
  SEXP out = vec_init(ptype, p.size);
  PROTECT_WITH_INDEX(out, &out_prot_idx);
  out = vec_proxy(out);
  REPROTECT(out, out_prot_idx);

  // The indices to slice x with
  SEXP index = PROTECT(compact_seq(0, 0, true));
  int* p_index = INTEGER(index);
  Rf_defineVar(syms_index, index, env);

  // The result of each function call
  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  // The container that temporarily holds the results
  // of `vec_slice(x, index)` in the `slide()` case and
  // `list(vec_slice(x[[1]], index), vec_slice(x[[2]], index), ...)`
  // in the `slide2()` and `pslide()` cases
  SEXP slices = PROTECT(slice_container(p.type));
  SEXP* p_slices = &slices;

  int seq_start;
  int seq_end;
  int seq_size;

  for (int i = 0; i < n; ++i) {
    if (i % 1024 == 0) {
      R_CheckUserInterrupt();
    }

    if (p.forward) {
      seq_start = max(window_start, x_start);
      seq_end = min(window_end, x_end);
      seq_size = seq_end - seq_start + 1;
      init_compact_seq(p_index, seq_start, seq_size, true);
    } else {
      seq_start = min(window_start, x_start);
      seq_end = max(window_end, x_end);
      seq_size = seq_start - seq_end + 1;
      init_compact_seq(p_index, seq_start, seq_size, false);
    }

    // Update the `f_call` variables in `env`
    update_slices(p_slices, x, index, env, p.type);

    elt = Rf_eval(f_call, env);
    REPROTECT(elt, elt_prot_idx);

    // TODO - Worry about needing fallback method when no proxy is defined / is a matrix
    // https://github.com/r-lib/vctrs/blob/8d12bfc0e29e056966e0549af619253253752a64/src/slice-assign.c#L46

    if (p.constrain) {
      elt = vctrs_cast(elt, ptype, strings_empty, strings_empty);
      REPROTECT(elt, elt_prot_idx);
      elt = vec_proxy(elt);
      REPROTECT(elt, elt_prot_idx);

      if (vec_size(elt) != 1) {
        Rf_errorcall(R_NilValue, "Incompatible lengths: %i, %i", vec_size(elt), 1);
      }

      vec_assign_impl(out, entry, elt, false);
    } else {
      SET_VECTOR_ELT(out, *p_entry - 1, elt);
    }

    window_start += window_start_step;
    window_end += window_end_step;
    *p_entry += entry_step;
  }

  out = vec_restore(out, ptype, r_int(p.size));
  REPROTECT(out, out_prot_idx);

  out = copy_names(out, x, p.type);
  REPROTECT(out, out_prot_idx);

  UNPROTECT(5);
  return out;
}

// -----------------------------------------------------------------------------

SEXP slurrr_slide(SEXP x, SEXP f_call, SEXP ptype, SEXP env, SEXP param_list) {
  struct slide_params params = init_params(x, param_list);
  return slide(x, f_call, ptype, env, params);
}

// -----------------------------------------------------------------------------

static SEXP copy_names(SEXP out, SEXP x, int type) {
  SEXP names;
  if (type == SLIDE) {
    names = PROTECT(vec_names(x));
  } else {
    names = PROTECT(vec_names(VECTOR_ELT(x, 0)));
  }

  UNPROTECT(1);
  return vec_set_names(out, names);
}

// -----------------------------------------------------------------------------

// update_slices() works by repeatedly overwriting the `slices` SEXP with the
// slices from `x`. If we are calling slide() or slide2(), it just overwrites
// `slices` directly and immediately assigns the result into an environment.
// If we are calling pslide(), then `slices` is a list and each element of the
// list is overwritten with the current slice of the i-th pslide element.
// Then that entire list is defined in the environment.

static SEXP slice_container(int type) {
  if (type == SLIDE || type == SLIDE2) {
    return R_NilValue;
  }

  return Rf_allocVector(VECSXP, type);
}

static void update_slices(SEXP* p_slices, SEXP x, SEXP index, SEXP env, int type) {
  // slide()
  if (type == SLIDE) {
    *p_slices = vec_slice_impl(x, index);
    Rf_defineVar(syms_dot_x, *p_slices, env);
    return;
  }

  // slide2()
  if (type == SLIDE2) {
    *p_slices = vec_slice_impl(VECTOR_ELT(x, 0), index);
    Rf_defineVar(syms_dot_x, *p_slices, env);
    *p_slices = vec_slice_impl(VECTOR_ELT(x, 1), index);
    Rf_defineVar(syms_dot_y, *p_slices, env);
    return;
  }

  SEXP slice;

  // pslide()
  for (int i = 0; i < type; ++i) {
    slice = vec_slice_impl(VECTOR_ELT(x, i), index);
    SET_VECTOR_ELT(*p_slices, i, slice);
  }

  Rf_defineVar(syms_dot_l, *p_slices, env);
}

// -----------------------------------------------------------------------------

// It is possible to set a combination of .offset/.after/.complete such that
// this difference ends up out of bounds so we pin it to 0L if that is the case.
static int compute_iterations(int x_end, int loc, int step, bool forward) {
  int diff = x_end - loc;

  if (!forward) {
    diff *= -1;
  }

  // Purposeful integer division
  int n_iter = (diff / step) + 1;

  return max(n_iter, 0);
}

static int iterations(int x_start, int x_end, const struct slide_params params) {

  int frame_pos_adjustment;

  if (params.forward) {
    frame_pos_adjustment = params.offset;
  } else {
    frame_pos_adjustment = -params.offset;
  }

  int frame_boundary_adjustment;

  if (!params.complete) {
    // boundary = start of frame
    if (params.forward) {
      frame_boundary_adjustment = -params.before;
    } else {
      frame_boundary_adjustment = params.after;
    }
  } else {
    // boundary = end of frame
    if (params.forward) {
      frame_boundary_adjustment = params.after;
    } else {
      frame_boundary_adjustment = -params.before;
    }
  }

  int frame_pos = x_start + frame_pos_adjustment;
  int frame_boundary = frame_pos + frame_boundary_adjustment;

  int n_iter_frame_pos = compute_iterations(x_end, frame_pos, params.step, params.forward);
  int n_iter_frame_boundary = compute_iterations(x_end, frame_boundary, params.step, params.forward);

  return min(n_iter_frame_pos, n_iter_frame_boundary);
}
