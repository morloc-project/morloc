#ifndef __BUILD_PATH_H__
#define __BUILD_PATH_H__

#include "ws_access.h"
#include "build_io.h" // needed for composon_inputs/composon_outputs

void resolve_grprefs(Ws* ws);

void resolve_refers(Ws* ws);

void resolve_derefs(Ws* ws);

#endif
