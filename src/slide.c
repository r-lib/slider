#include "slurrr.h"
#include "utils.h"
#include <vctrs.h>

#define SLIDE -1
#define SLIDE2 -2

// -----------------------------------------------------------------------------
// All defined below

void validate_before_after_negativeness(int before, int after);

SEXP slice_container(int n);

void slice_loop(SEXP* p_slices, SEXP x, SEXP index, SEXP env, int n);

SEXP copy_names(SEXP out, SEXP x, int n);

// -----------------------------------------------------------------------------

bool is_unbounded(SEXP x) {
  return Rf_inherits(x, "slurrr_box_unbounded");
}

bool is_scalar(SEXP x) {
  return vec_size(x) == 1;
}

void stop_scalar(const char * arg, int size) {
  Rf_errorcall(R_NilValue, "`%s` must have size 1, not %i.", arg, size);
}

void check_scalar(SEXP x, SEXP arg) {
  if (is_scalar(x)) {
    return;
  }

  stop_scalar(r_scalar_chr_get(arg), vec_size(x));
}

SEXP check_scalar_ptype(SEXP x, SEXP ptype, SEXP x_arg) {
  x = PROTECT(vctrs_cast(
    x,
    ptype,
    x_arg,
    strings_empty
  ));

  check_scalar(x, x_arg);

  UNPROTECT(1);
  return x;
}

SEXP check_scalar_int(SEXP x, SEXP x_arg) {
  return check_scalar_ptype(x, slurrr_shared_empty_int, x_arg);
}

SEXP check_scalar_lgl(SEXP x, SEXP x_arg) {
  return check_scalar_ptype(x, slurrr_shared_empty_lgl, x_arg);
}

// -----------------------------------------------------------------------------

struct slide_params {
  int type;
  int size;
  bool constrain;
  int before;
  int after;
  int step;
  int offset;
  bool complete;
  bool forward;
  bool before_unbounded;
  bool after_unbounded;
};

void update_before(SEXP x, struct slide_params* params) {
  SEXP before = r_lst_get(x, 3);

  if (is_unbounded(before)) {
    params->before = 0;
    params->before_unbounded = true;
    return;
  }

  before = PROTECT(check_scalar_int(before, strings_dot_before));

  params->before = r_scalar_int_get(before);
  params->before_unbounded = false;

  UNPROTECT(1);
}

void update_after(SEXP x, struct slide_params* params) {
  SEXP after = r_lst_get(x, 4);

  if (is_unbounded(after)) {
    params->after = 0;
    params->after_unbounded = true;
    return;
  }

  after = PROTECT(check_scalar_int(after, strings_dot_after));

  params->after = r_scalar_int_get(after);
  params->after_unbounded = false;

  UNPROTECT(1);
}

void update_step(SEXP x, struct slide_params* params) {
  SEXP step = r_lst_get(x, 5);

  step = PROTECT(check_scalar_int(step, strings_dot_step));

  params->step = r_scalar_int_get(step);

  if (params->step < 1) {
    Rf_errorcall(R_NilValue, "`.step` must be at least 1, not %i.", params->step);
  }

  UNPROTECT(1);
}

void update_complete(SEXP x, struct slide_params* params) {
  SEXP complete = r_lst_get(x, 6);

  complete = PROTECT(check_scalar_lgl(complete, strings_dot_complete));

  params->complete = r_scalar_lgl_get(complete);

  UNPROTECT(1);
}

void update_forward(SEXP x, struct slide_params* params) {
  SEXP forward = r_lst_get(x, 7);

  forward = PROTECT(check_scalar_lgl(forward, strings_dot_forward));

  params->forward = r_scalar_lgl_get(forward);

  UNPROTECT(1);
}

void update_offset(SEXP x, struct slide_params* params) {
  SEXP offset = r_lst_get(x, 8);

  bool null_offset = offset == R_NilValue;

  if (!null_offset) {
    offset = PROTECT(check_scalar_int(offset, strings_dot_offset));
    params->offset = r_scalar_int_get(offset);
    UNPROTECT(1);
  }

  // - Checking if the start of the frame is out of range when going forward
  // - Ensure `before <= offset` so we can create a full window
  if (params->complete &&
      params->forward &&
      !params->before_unbounded &&
      (null_offset || params->before > params->offset)) {
    params->offset = params->before;
    return;
  }

  // - Checking if the start of the frame is out of range when going backward
  // - Ensure `after <= offset` so we can create a full window
  if (params->complete &&
      !params->forward &&
      !params->after_unbounded &&
      (null_offset || params->after > params->offset)) {
    params->offset = params->after;
    return;
  }

  // - Checking if the end of the frame is out of range when going forward
  // - Ensure that if `after < 0`, then `abs(after) <= offset`
  //   so we have some data to partially compute on
  if (!params->complete && params->forward && !params->after_unbounded && params->after < 0) {
    if (null_offset || params->offset < -params->after) {
      params->offset = -params->after;
      return;
    }
  }

  // - Checking if the end of the frame is out of range when going backward
  // - Ensure that if `before < 0`, then `abs(before) <= offset` so we have
  //   some data to partially compute on
  if (!params->complete && !params->forward && !params->before_unbounded && params->before < 0L) {
    if (null_offset || params->offset < -params->before) {
      params->offset = -params->before;
      return;
    }
  }

  // If offset was NULL and none of these conditions were met,
  // meaning that we have a usable window, then set offset to 0
  if (null_offset) {
    params->offset = 0;
  }

  return;
}

// The order of checks are important here and are done so they they work even
// if both .before/.after are unbounded(). The goal is to set unbounded()
// .before/.after values to values such that the width of the first
// iteration's window frame is correct
void update_before_after_if_unbounded(struct slide_params* params) {
  if (params->forward) {
    if (params->before_unbounded) {
      params->before = params->offset;
    }
    if (params->after_unbounded) {
      params->after = params->size - 1 - params->offset;
    }
  } else {
    if (params->after_unbounded) {
      params->after = params->offset;
    }
    if (params->before_unbounded) {
      params->before = params->size - 1 - params->offset;
    }
  }
}

struct slide_params init_params(SEXP x) {
  struct slide_params params;

  params.type = r_scalar_int_get(r_lst_get(x, 0));
  params.size = r_scalar_int_get(r_lst_get(x, 1));
  params.constrain = r_scalar_lgl_get(r_lst_get(x, 2));

  update_before(x, &params);
  update_after(x, &params);
  update_step(x, &params);
  update_complete(x, &params);
  update_forward(x, &params);

  // Must be done after all of: before/after/complete/forward
  update_offset(x, &params);

  update_before_after_if_unbounded(&params);
  validate_before_after_negativeness(params.before, params.after);

  return params;
}

// -----------------------------------------------------------------------------

int iterations(int x_start,
               int x_end,
               const struct slide_params params);

// -----------------------------------------------------------------------------

SEXP slide(SEXP x,
           SEXP f_call,
           SEXP ptype,
           SEXP env,
           struct slide_params p) {

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

  // We check to see if we are unbounded() in the direction that we are sliding.
  // If so, we force `.complete = FALSE` to compute the correct number of iterations
  // (This must be done after we compute the .offset, because the .complete-ness
  // does affect the partial results at the beginning)
  if ((p.after_unbounded && p.forward) || (p.before_unbounded && !p.forward)) {
    p.complete = false;
  }

  int n_iter = iterations(
    x_start,
    x_end,
    p
  );

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
  Rf_defineVar(Rf_install("index"), index, env);

  // The result of each function call
  PROTECT_INDEX elt_prot_idx;
  SEXP elt = R_NilValue;
  PROTECT_WITH_INDEX(elt, &elt_prot_idx);

  // The container you get from slicing all elements of `x`
  SEXP slices = PROTECT(slice_container(p.type));
  SEXP* p_slices = &slices;

  int seq_start;
  int seq_end;
  int seq_size;

  for (int i = 0; i < n_iter; ++i) {
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

    slice_loop(p_slices, x, index, env, p.type);

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

  out = vec_restore(out, ptype, R_NilValue);
  REPROTECT(out, out_prot_idx);

  out = copy_names(out, x, p.type);
  REPROTECT(out, out_prot_idx);

  UNPROTECT(5);
  return out;
}

// -----------------------------------------------------------------------------

SEXP slurrr_slide(SEXP x, SEXP f_call, SEXP ptype, SEXP env, SEXP param_list) {
  struct slide_params params = init_params(param_list);
  return slide(x, f_call, ptype, env, params);
}

// -----------------------------------------------------------------------------

// Sets `names` on `x` in the vctrs style
// Will make a copy of `x` as necessary to be consistent
// with the fallback `names()<-` methods

SEXP vec_set_names(SEXP x, SEXP names) {
  if (names == R_NilValue) {
    return x;
  }

  // Never on a data frame
  if (OBJECT(x) && Rf_inherits(x, "data.frame")) {
    return x;
  }

  // rownames(x) <- names
  if (vec_dim_n(x) > 1) {
    SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 3));

    Rf_defineVar(syms_set_rownames, fns_set_rownames, env);
    Rf_defineVar(syms_x, x, env);
    Rf_defineVar(syms_names, names, env);

    SEXP call = PROTECT(Rf_lang3(syms_set_rownames, syms_x, syms_names));

    UNPROTECT(2);
    return Rf_eval(call, env);
  }

  // names(x) <- names
  if (OBJECT(x)) {
    SEXP env = PROTECT(r_new_environment(R_GlobalEnv, 3));

    Rf_defineVar(syms_set_names, fns_set_names, env);
    Rf_defineVar(syms_x, x, env);
    Rf_defineVar(syms_names, names, env);

    SEXP call = PROTECT(Rf_lang3(syms_set_names, syms_x, syms_names));

    UNPROTECT(2);
    return Rf_eval(call, env);
  }

  x = PROTECT(r_maybe_duplicate(x));

  Rf_setAttrib(x, R_NamesSymbol, names);

  UNPROTECT(1);
  return x;
}

SEXP copy_names(SEXP out, SEXP x, int n) {
  SEXP names;
  if (n == SLIDE) {
    names = PROTECT(vec_names(x));
  } else {
    names = PROTECT(vec_names(VECTOR_ELT(x, 0)));
  }

  UNPROTECT(1);
  return vec_set_names(out, names);
}

// -----------------------------------------------------------------------------

// The slice_loop() works by repeatedly overwriting the `slices` SEXP with the
// slices from `x`. If we are calling slide() or slide2(), it just overwrites
// `slices` directly and immediately assigns the result into an environment.
// If we are calling pslide(), then `slices` is a list and each element of the
// list is overwritten with the current slice of the i-th pslide element.
// Then that entire list is defined in the environment.

SEXP slice_container(int n) {
  if (n == SLIDE || n == SLIDE2) {
    return R_NilValue;
  }

  return Rf_allocVector(VECSXP, n);
}

void slice_loop(SEXP* p_slices, SEXP x, SEXP index, SEXP env, int n) {
  // slide()
  if (n == SLIDE) {
    *p_slices = vec_slice_impl(x, index);
    Rf_defineVar(syms_dot_x, *p_slices, env);
    return;
  }

  // slide2()
  if (n == SLIDE2) {
    *p_slices = vec_slice_impl(VECTOR_ELT(x, 0), index);
    Rf_defineVar(syms_dot_x, *p_slices, env);
    *p_slices = vec_slice_impl(VECTOR_ELT(x, 1), index);
    Rf_defineVar(syms_dot_y, *p_slices, env);
    return;
  }

  SEXP slice;

  // pslide()
  for (int i = 0; i < n; ++i) {
    slice = vec_slice_impl(VECTOR_ELT(x, i), index);
    SET_VECTOR_ELT(*p_slices, i, slice);
  }

  Rf_defineVar(syms_dot_l, *p_slices, env);
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

int iterations(int x_start,
               int x_end,
               const struct slide_params params) {

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


#undef SLIDE
#undef SLIDE2
