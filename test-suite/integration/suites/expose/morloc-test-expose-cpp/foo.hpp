#ifndef MORLOC_TEST_EXPOSE_FOO_HPP
#define MORLOC_TEST_EXPOSE_FOO_HPP

// Relative include from one exposed header to another. Resolves via
// the includer's directory rule once both files are co-located under
// <MORLOC_HOME>/include/morloc-test-expose-cpp/.
#include "foo_helpers/util.hpp"

namespace mlc_test {
    inline int foo(int x) {
        return util_add_one(x) * 2;
    }
}

#endif
