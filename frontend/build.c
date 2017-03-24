#include "build.h"

void build_manifolds(Ws* ws_top){

    resolve_grprefs(ws_top);

    resolve_derefs(ws_top);

    resolve_refers(ws_top);

    link_modifiers(ws_top);

    propagate_nargs(ws_top);

    set_as_function(ws_top);

    link_inputs(ws_top);

    set_default_types(ws_top);

    /* infer_multi_types(ws_top);   */
    /*                              */
    /* infer_star_types(ws_top);    */
    /*                              */
    /* infer_generic_types(ws_top); */
}
