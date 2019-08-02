#include "slurrr.h"

SEXP strings_empty = NULL;

SEXP syms_dot_x = NULL;
SEXP syms_dot_y = NULL;
SEXP syms_dot_l = NULL;
SEXP syms_x = NULL;
SEXP syms_names = NULL;

SEXP syms_set_names = NULL;
SEXP syms_set_rownames = NULL;

SEXP fns_set_names = NULL;
SEXP fns_set_rownames = NULL;

// -----------------------------------------------------------------------------

int r_int_get(SEXP x, int i) {
  return INTEGER(x)[i];
}

bool r_lgl_get(SEXP x, int i) {
  return LOGICAL(x)[i];
}

// -----------------------------------------------------------------------------

SEXP r_maybe_duplicate(SEXP x) {
  if (MAYBE_REFERENCED(x)) {
    return Rf_shallow_duplicate(x);
  } else {
    return x;
  }
}

// -----------------------------------------------------------------------------

// r_new_environment() from vctrs utils.c

#include <R_ext/Parse.h>

static void abort_parse(SEXP code, const char* why) {
  if (Rf_GetOption1(Rf_install("rlang__verbose_errors")) != R_NilValue) {
    Rf_PrintValue(code);
  }
  Rf_error("Internal error: %s", why);
}

SEXP r_parse(const char* str) {
  SEXP str_ = PROTECT(Rf_mkString(str));

  ParseStatus status;
  SEXP out = PROTECT(R_ParseVector(str_, -1, &status, R_NilValue));
  if (status != PARSE_OK) {
    abort_parse(str_, "Parsing failed");
  }
  if (Rf_length(out) != 1) {
    abort_parse(str_, "Expected a single expression");
  }

  out = VECTOR_ELT(out, 0);

  UNPROTECT(2);
  return out;
}
SEXP r_parse_eval(const char* str, SEXP env) {
  SEXP out = Rf_eval(PROTECT(r_parse(str)), env);
  UNPROTECT(1);
  return out;
}

static SEXP new_env_call = NULL;
static SEXP new_env__parent_node = NULL;
static SEXP new_env__size_node = NULL;

SEXP r_new_environment(SEXP parent, R_len_t size) {
  parent = parent ? parent : R_EmptyEnv;
  SETCAR(new_env__parent_node, parent);

  size = size ? size : 29;
  SETCAR(new_env__size_node, Rf_ScalarInteger(size));

  SEXP env = Rf_eval(new_env_call, R_BaseEnv);

  // Free for gc
  SETCAR(new_env__parent_node, R_NilValue);

  return env;
}

// -----------------------------------------------------------------------------

// [[register()]]
void slurrr_init_utils() {
  syms_dot_x = Rf_install(".x");
  syms_dot_y = Rf_install(".y");
  syms_dot_l = Rf_install(".l");
  syms_x = Rf_install("x");
  syms_names = Rf_install("names");

  syms_set_names = Rf_install("names<-");
  fns_set_names = Rf_findVar(syms_set_names, R_BaseEnv);

  syms_set_rownames = Rf_install("rownames<-");
  fns_set_rownames = Rf_findVar(syms_set_rownames, R_BaseEnv);

  strings_empty = Rf_allocVector(STRSXP, 1);
  R_PreserveObject(strings_empty);
  SET_STRING_ELT(strings_empty, 0, Rf_mkChar(""));

  new_env_call = r_parse_eval("as.call(list(new.env, TRUE, NULL, NULL))", R_BaseEnv);
  R_PreserveObject(new_env_call);

  new_env__parent_node = CDDR(new_env_call);
  new_env__size_node = CDR(new_env__parent_node);
}
