#include "build.h"

void build_manifolds(Ws* ws_top){

    resolve_grprefs(ws_top);

    resolve_derefs(ws_top);

    resolve_refers(ws_top);

    link_modifiers(ws_top);

    link_inputs(ws_top);

}
