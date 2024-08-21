#include "tree-parser.h"

using namespace cpp11;

[[cpp11::register]]
SEXP cpp_parse_one_file() {
    Rprintf("Yep\n");
    return(R_NilValue);
}
