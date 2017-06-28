#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP simstudy_vecMultinom(SEXP probsSEXP);
extern SEXP simstudy_matMultinom(SEXP probmatrixSEXP);

static const R_CallMethodDef CallEntries[] = {
  {"simstudy_matMultinom", (DL_FUNC) &simstudy_matMultinom, 1},
  {"simstudy_vecMultinom", (DL_FUNC) &simstudy_vecMultinom, 1},
  {NULL, NULL, 0}
};

void R_init_simstudy(DllInfo *info)
{
  R_registerRoutines(info, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(info, TRUE);
}
