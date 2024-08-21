#include "tree-parser.h"

using namespace cpp11;

[[cpp11::register]]
std::string cpp_parse_one_file() {
    Rprintf("Yep\n");

    writable::strings s({"foo", "bar"});
    std::string out = cpp11::r_string(s[0]);
    return(out);
}
