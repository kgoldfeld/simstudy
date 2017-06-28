#include <R.h>
#include <Rinternals.h> // for SEXP
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP simstudy_vecMultinom(SEXP probsSEXP);
extern SEXP simstudy_matMultinom(SEXP probmatrixSEXP);

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
  CALLDEF(simstudy_matMultinom, 1),
  CALLDEF(simstudy_vecMultinom, 1),
  {NULL, NULL, 0}
};

void R_init_simstudy(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, R_CallDef, NULL, NULL);
  R_useDynamicSymbols(dll, TRUE);
}
