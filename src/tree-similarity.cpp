#include "cpp11.hpp"

using namespace cpp11;

[[cpp11::register]]
writable::strings cpp_tree_similarity(strings x) {
    int n = x.size();
    writable::strings out(n);
    for (int i = 0; i < n; i++) out[i] = "a";

    std::string s = r_string(x[0]);
    Rprintf("%s\n", s.c_str());

    return out;
}
