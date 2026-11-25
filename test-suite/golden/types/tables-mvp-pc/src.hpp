#ifndef SRC_HPP
#define SRC_HPP
#include "mlc_arrow.hpp"

// Hand the received table straight back; the pool passes it through.
inline mlc::ArrowTable idCpp(const mlc::ArrowTable& t) {
    return t;
}
#endif
