#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP c_parse_one_file(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"c_parse_one_file", (DL_FUNC) &c_parse_one_file, 1},
    {NULL, NULL, 0}
};

void R_init_pkgsimil(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
