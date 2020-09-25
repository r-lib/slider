#ifndef SLIDER_PARAMS_H
#define SLIDER_PARAMS_H

#include "slider.h"

int validate_type(SEXP x);
bool validate_constrain(SEXP x);
bool validate_atomic(SEXP x);
int validate_before(SEXP x, bool* before_unbounded);
int validate_after(SEXP x, bool* after_unbounded);
int validate_step(SEXP x);
int validate_complete(SEXP x);

void check_double_negativeness(int before, int after, bool before_positive, bool after_positive);
void check_after_negativeness(int after, int before, bool after_positive, bool before_unbounded);
void check_before_negativeness(int before, int after, bool before_positive, bool after_unbounded);

#endif
