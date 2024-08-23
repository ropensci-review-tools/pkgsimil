#include "cpp11.hpp"

using namespace cpp11;

[[cpp11::register]]
writable::integers cpp_tree_similarity(strings x) {
    int n = x.size();
    if (n < 2) {
        cpp11::stop("tree_similarity requires at least 2 trees\n");
    }

    std::string tree1 = r_string(x[0]);
    std::string tree2 = r_string(x[1]);

    writable::integers out(n);
    for (int i = 0; i < n; i++) out[i] = i;

    return out;
}
