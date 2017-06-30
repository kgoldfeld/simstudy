#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/*
The following symbols/expressions for .NAME have been omitted

simstudy_vecMultinom
simstudy_matMultinom

Most likely possible values need to be added below.
*/

/* FIXME:
Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP simstudy_vecMultinom(SEXP probsSEXP);
extern SEXP simstudy_matMultinom(SEXP probmatrixSEXP);

static const R_CallMethodDef CallMethods[] = {
  {"simstudy_matMultinom", (DL_FUNC) &simstudy_matMultinom, 1},
  {"simstudy_vecMultinom", (DL_FUNC) &simstudy_vecMultinom, 1},
  {NULL, NULL, 0}
};

void R_init_simstudy(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallMethods, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);

}

