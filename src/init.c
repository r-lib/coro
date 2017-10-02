#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP rlang_poke_attr(SEXP, SEXP, SEXP);

static const R_CallMethodDef call_entries[] = {
  {"rlang_poke_attr",    (DL_FUNC) &rlang_poke_attr, 3},
  {NULL, NULL, 0}
};

void R_init_flowery(DllInfo* dll) {
  R_registerRoutines(dll, NULL, call_entries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
