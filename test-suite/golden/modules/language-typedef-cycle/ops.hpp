// Trivial identity helper used by every intra/param/cross variant
// that imports Cpp. The body never runs (compile fails first); this
// file only exists so morloc's source resolver doesn't reject the
// signature before it gets a chance to walk the cyclic typedefs.
template<typename T> T identity(T x) { return x; }
