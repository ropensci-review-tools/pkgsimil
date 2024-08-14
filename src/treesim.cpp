#include "treesim.h"

using namespace cpp11;

[[cpp11::register]]
writable::integers cpp_test(const strings flist) {
    const int n = static_cast <int> (flist.size ());

    writable::integers res (n);
    std::fill (res.begin (), res.end (), 0L);

    for (int i = 0; i < n; i++) {
        res [i] = i;
    }

    return res;
}
