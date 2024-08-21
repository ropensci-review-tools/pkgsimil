#include "tree-parser.h"

using namespace cpp11;

[[cpp11::register]]
SEXP parse_one_file() {
    printf("Yep\n");
    return(R_NilValue);
}
