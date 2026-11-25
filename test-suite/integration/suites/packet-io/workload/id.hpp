// C++ identity, exercised by the packet-io cross-pool tier. The
// templated form lets morloc instantiate it for any incoming type.

#ifndef PACKET_IO_ID_HPP
#define PACKET_IO_ID_HPP

template <typename T>
T ident_cpp(T x) {
    return x;
}

#endif
