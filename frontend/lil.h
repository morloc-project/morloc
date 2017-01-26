#ifndef __LIL_H__
#define __LIL_H__

#include "ws_access.h"


// LIL instruction strings
// -----------------------
// NOTE: changes must be synced with backends/parse-grammar.awk or possibly
// backends/build.sh. 
#define LIL_SOURCE           "NSRC"
#define LIL_EXPORT           "EXPT"
#define LIL_EMIT             "EMIT"
#define LIL_FUNCTION         "FUNC"
#define LIL_TYPE             "TYPE"
#define LIL_MANIFOLD_INPUT   "INPM"
#define LIL_POSITIONAL_INPUT "INPP"
#define LIL_FUNCTION_INPUT   "INPF"
#define LIL_ARG_INPUT        "INPA"
#define LIL_N_MANIFOLD_ARGS  "NARG"
#define LIL_CHECK            "CHEK"
#define LIL_FAIL             "FAIL"
#define LIL_CACHE            "CACH"
#define LIL_MANIFOLD_DOC     "MDOC"
#define LIL_FUNCTION_ARG     "FARG"
#define LIL_HOOK             "HOOK"

void print_lil(Ws* lil);

Ws* build_lil(Ws* t);

#endif
