#include "slurrr.h"
#include "utils.h"
#include <vctrs.h>

// -----------------------------------------------------------------------------
// All defined below

int compute_offset(SEXP offset_,
                   int before,
                   int after,
                   bool before_unbounded,
                   bool after_unbounded,
                   bool complete,
                   bool forward);

void validate_before_after_negativeness(int before, int after);

int iterations2(int x_start,
               int x_end,
               int before,
               int after,
               int step,
               int offset,
               bool complete,
               bool forward);

SEXP slice_container(int n);

void slice_loop(SEXP* p_slice, SEXP x, SEXP index, SEXP env, int n);

// -----------------------------------------------------------------------------

// just to test if it works
SEXP slurrr_slide2(
                  SEXP x,
                  SEXP inputs_,
                  SEXP f_call,
                  SEXP size_,
                  SEXP before_,
                  SEXP after_,
                  SEXP step_,
                  SEXP offset_,
                  SEXP complete_,
                  SEXP forward_,
                  SEXP ptype,
                  SEXP before_unbounded_,
                  SEXP after_unbounded_,
                  SEXP constrain_,
                  SEXP env) {

  int inputs = r_int_get(inputs_, 0);
  int size = r_int_get(size_, 0);
  int step = r_int_get(step_, 0);
  bool complete = r_lgl_get(complete_, 0);
  bool forward = r_lgl_get(forward_, 0);
  bool before_unbounded = r_lgl_get(before_unbounded_, 0);
  bool after_unbounded = r_lgl_get(after_unbounded_, 0);
  bool constrain = r_lgl_get(constrain_, 0);

  int before = 0;
  if (!before_unbounded) {
    before = r_int_get(before_, 0);
  }

  int after = 0;
  if (!after_unbounded) {
    after = r_int_get(after_, 0);
  }

  int offset = compute_offset(
    offset_,
    before,
    after,
    before_unbounded,
    after_unbounded,
    complete,
    forward
  );

  // The order of checks are important here and are done so they they work even
  // if both .before/.after are unbounded(). The goal is to set unbounded()
  // .before/.after values to values such that the width of the first
  // iteration's window frame is correct
  if (forward) {
    if (before_unbounded) {
      before = offset;
    }
    if (after_unbounded) {
      after = size - 1 - offset;
    }
  } else {
    if (after_unbounded) {
      after = offset;
    }
    if (before_unbounded) {
      before = size - 1 - offset;
    }
  }

  validate_before_after_negativeness(before, after);

  int x_start;
  int x_end;
  int window_start;
  int window_end;
  int entry_step;
  int entry_offset;

  if (forward) {
    x_start = 0;
    x_end = size - 1;
    window_start = x_start - before + offset;
    window_end = window_start + (before + after);
    entry_step = step;
    entry_offset = offset;
  }
  else {
    x_start = size - 1;
    x_end = 0;
    window_start = x_start + after - offset;
    window_end = window_start - (before + after);
    entry_step = -step;
    entry_offset = -offset;
  }

  // `entry` has to be 1-based for `vec_assign_impl()`
  SEXP entry = PROTECT(r_int(x_start + entry_offset + 1));
  int* p_entry = INTEGER(entry);

  // We check to see if we are unbounded() in the direction that we are sliding.
  // If so, we force `.complete = FALSE` to compute the correct number of iterations
  // (This must be done after we compute the .offset, because the .complete-ness
  // does affect the partial results at the beginning)
  if ((after_unbounded && forward) || (before_unbounded && !forward)) {
    complete = false;
  }

  int n_iter = iterations2(
    x_start,
    x_end,
    before,
    after,
    step,
    offset,
    complete,
    forward
  );

  int window_start_step = entry_step;
  int window_end_step = entry_step;
  if (forward) {
    if (before_unbounded) {
      window_start_step = 0;
    }
    if (after_unbounded) {
      window_end_step = 0;
    }
  } else {
    if (before_unbounded) {
      window_end_step = 0;
    }
    if (after_unbounded) {
      window_start_step = 0;
    }
  }

  // Init and proxy our `out` container
  PROTECT_INDEX out_prot_idx;
  SEXP out = vec_init(ptype, size);
  PROTECT_WITH_INDEX(out, &out_prot_idx);
  out = vec_proxy(out);
  REPROTECT(out, out_prot_idx);

  // The indices to slice x with
  SEXP index = PROTECT(compact_seq(0, 0, true));
  int* p_index = INTEGER(index);
  Rf_defineVar(Rf_install("index"), index, env);

  // The result of each function call
  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  // The current slice of x, defined as syms_slice in the env
  PROTECT_INDEX slice_prot_idx;
  SEXP slice = slice_container(inputs);
  PROTECT_WITH_INDEX(slice, &slice_prot_idx);
  SEXP* p_slice = &slice;

  int seq_start;
  int seq_end;
  int seq_size;

  for (int i = 0; i < n_iter; ++i) {

    if (forward) {
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

    slice_loop(p_slice, x, index, env, inputs);

    elt = Rf_eval(f_call, env);
    REPROTECT(elt, elt_prot_idx);

    if (constrain) {
      elt = vctrs_cast(elt, ptype, strings_empty, strings_empty);
      REPROTECT(elt, elt_prot_idx);
      elt = vec_proxy(elt);
      REPROTECT(elt, elt_prot_idx);

      vec_assign_impl(out, entry, elt, false);
    } else {
      SET_VECTOR_ELT(out, *p_entry - 1, elt);
    }

    window_start += window_start_step;
    window_end += window_end_step;
    *p_entry += step;
  }

  UNPROTECT(5);
  return vec_restore(out, ptype, R_NilValue);
}

// -----------------------------------------------------------------------------

#define SLIDE -1
#define SLIDE2 -2

SEXP slice_container(int n) {
  if (n == SLIDE || n == SLIDE2) {
    return R_NilValue;
  }

  return Rf_allocVector(VECSXP, n);
}

void slice_loop(SEXP* p_slice, SEXP x, SEXP index, SEXP env, int n) {
  // slide()
  if (n == SLIDE) {
    *p_slice = vec_slice_impl(x, index);
    Rf_defineVar(syms_slice, *p_slice, env);
    return;
  }

  // slide2()
  if (n == SLIDE2) {
    *p_slice = vec_slice_impl(VECTOR_ELT(x, 0), index);
    Rf_defineVar(syms_slice, *p_slice, env);
    *p_slice = vec_slice_impl(VECTOR_ELT(x, 1), index);
    Rf_defineVar(syms_slice2, *p_slice, env);
    return;
  }

  SEXP elt;

  // pslide()
  for (int i = 0; i < n; ++i) {
    elt = vec_slice_impl(VECTOR_ELT(x, i), index);
    SET_VECTOR_ELT(*p_slice, i, elt);
  }

  Rf_defineVar(syms_slice, *p_slice, env);
}

#undef SLIDE
#undef SLIDE2

// -----------------------------------------------------------------------------

int compute_offset(SEXP offset_,
                   int before,
                   int after,
                   bool before_unbounded,
                   bool after_unbounded,
                   bool complete,
                   bool forward) {

  bool null_offset = offset_ == R_NilValue;

  int offset;
  if (!null_offset) {
    offset = r_int_get(offset_, 0);
  }

  // - Checking if the start of the frame is out of range when going forward
  // - Ensure `before <= offset` so we can create a full window
  if (complete && forward && !before_unbounded && (null_offset || before > offset)) {
    return(before);
  }

  // - Checking if the start of the frame is out of range when going backward
  // - Ensure `after <= offset` so we can create a full window
  if (complete && !forward && !after_unbounded && (null_offset || after > offset)) {
    return(after);
  }

  // - Checking if the end of the frame is out of range when going forward
  // - Ensure that if `after < 0`, then `abs(after) <= offset`
  //   so we have some data to partially compute on
  if (!complete && forward && !after_unbounded && after < 0L) {
    if (null_offset || offset < -after) {
      return(-after);
    }
  }

  // - Checking if the end of the frame is out of range when going backward
  // - Ensure that if `before < 0`, then `abs(before) <= offset` so we have
  //   some data to partially compute on
  if (!complete && !forward && !before_unbounded && before < 0L) {
    if (null_offset || offset < -before) {
      return(-before);
    }
  }

  // If offset was NULL and none of these conditions were met,
  // meaning that we have a usable window, then set offset to 0
  if (null_offset) {
    return(0);
  }

  return offset;
}

// -----------------------------------------------------------------------------

void validate_before_after_negativeness(int before, int after) {
  bool before_negative = before < 0;
  bool after_negative = after < 0;

  if (!before_negative && !after_negative) {
    return;
  }

  if (before_negative && after_negative) {
    Rf_errorcall(
      R_NilValue,
      "`.before` (%i) and `.after` (%i) cannot both be negative.",
      before,
      after
    );
  }

  if (before_negative && abs(before) > after) {
    int abs_before = abs(before);
    Rf_errorcall(
      R_NilValue,
      "When `.before` (%i) is negative, it's absolute value (%i) cannot be greater than `.after` (%i).",
      before,
      abs_before,
      after
    );
  }

  if (after_negative && abs(after) > before) {
    int abs_after = abs(after);
    Rf_errorcall(
      R_NilValue,
      "When `.after` (%i) is negative, it's absolute value (%i) cannot be greater than `.before` (%i).",
      after,
      abs_after,
      before
    );
  }
}

// -----------------------------------------------------------------------------

// It is possible to set a combination of .offset/.after/.complete such that
// this difference ends up out of bounds so we pin it to 0L if that is the case.
int compute_iterations(int x_end, int loc, int step, bool forward) {
  int diff = x_end - loc;

  if (!forward) {
    diff *= -1;
  }

  // Purposeful integer division
  int n_iter = (diff / step) + 1;

  return max(n_iter, 0);
}

int iterations2(int x_start,
               int x_end,
               int before,
               int after,
               int step,
               int offset,
               bool complete,
               bool forward) {

  int frame_pos_adjustment;

  if (forward) {
    frame_pos_adjustment = offset;
  } else {
    frame_pos_adjustment = -offset;
  }

  int frame_boundary_adjustment;

  if (!complete) {
    // boundary = start of frame
    if (forward) {
      frame_boundary_adjustment = -before;
    } else {
      frame_boundary_adjustment = after;
    }
  } else {
    // boundary = end of frame
    if (forward) {
      frame_boundary_adjustment = after;
    } else {
      frame_boundary_adjustment = -before;
    }
  }

  int frame_pos = x_start + frame_pos_adjustment;
  int frame_boundary = frame_pos + frame_boundary_adjustment;

  int n_iter_frame_pos = compute_iterations(x_end, frame_pos, step, forward);
  int n_iter_frame_boundary = compute_iterations(x_end, frame_boundary, step, forward);

  return min(n_iter_frame_pos, n_iter_frame_boundary);
}
