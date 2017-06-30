#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* .Call calls */

extern SEXP simstudy_matMultinom(SEXP probmatrixSEXP);

static const R_CallMethodDef CallMethods[] = {
  {"simstudy_matMultinom", (DL_FUNC) &simstudy_matMultinom, 1},
  {NULL, NULL, 0}
};

void R_init_simstudy(DllInfo *dll)
{

  R_registerRoutines(dll, NULL, CallMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

}
