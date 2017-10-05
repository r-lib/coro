#define R_NO_REMAP
#include <Rinternals.h>

SEXP rlang_remove_attr(SEXP x, SEXP name) {
  SEXP attrs = ATTRIB(x);
  if (attrs == R_NilValue) {
    return x;
  }

  attrs = PROTECT(Rf_shallow_duplicate(attrs));
  SEXP parent = R_NilValue;

  while (attrs != R_NilValue) {

    if (TAG(attrs) == name) {
      if (parent == R_NilValue) {
        SET_ATTRIB(x, CDR(attrs));
      } else {
        SETCDR(parent, CDR(attrs));
      }
      break;
    }

    parent = attrs;
    attrs = CDR(attrs);
  }

  UNPROTECT(1);
  return x;
}

SEXP rlang_poke_attr(SEXP x, SEXP name, SEXP value) {
  if (value == R_NilValue) {
    return rlang_remove_attr(x, name);
  }

  SEXP attrs = ATTRIB(x);

  if (attrs == R_NilValue) {
    attrs = PROTECT(Rf_cons(value, R_NilValue));
    SET_TAG(attrs, name);
    SET_ATTRIB(x, attrs);
    UNPROTECT(1);
    return x;
  }

  while (TRUE) {
    if (TAG(attrs) == name) {
      SETCAR(attrs, value);
      break;
    }

    if (CDR(attrs) == R_NilValue) {
      SEXP attr = PROTECT(Rf_cons(value, R_NilValue));
      SETCDR(attrs, attr);
      UNPROTECT(1);
      break;
    }

    attrs = CDR(attrs);
  }

  return x;
}
