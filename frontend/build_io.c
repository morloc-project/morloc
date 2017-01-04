#include "build_io.h"

// link all top level elements in c_{i+1} as inputs to c_i
void _link_pair(W* input, W* output){
    // I'm dealing with C_MANIFOLD's not P_MANIFOLD's
    // A C_MANIFOLD manifold is a couplet with a P_MANIFOLD rhs
    if(input->cls == C_MANIFOLD){
        Manifold* m = g_manifold(g_rhs(input));
        m->inputs = ws_add(m->inputs, o);
    }
}

void _link_composon(W* composon){

    // identify the elements within this composon which take input
    inputs = ws_composon_inputs(composon);

    // identify the elements in the next composon which produce output
    outputs = ws_composon_outputs(composon->next);

    // link each input to each output
    ws_mod2(inputs, outputs, _link_pair);
}

void link_inputs(Ws* ws_top){

    // gather all composons
    Ws* cs = ws_rfilter(ws_top, ws_recurse_most, w_is_composon);

    // pass each into _link_one
    ws_mod(cs, _link_composon);

}
