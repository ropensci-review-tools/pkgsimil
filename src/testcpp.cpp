#include "cpp11.hpp"

using namespace cpp11;

[[cpp11::register]]
cpp11::doubles cpp_test_fn(doubles x) {
    int n = x.size();
    writable::doubles out(n);
    for (int i = 0; i < n; i++) out[i] = x[i] + 2.0;

    return out;
}
