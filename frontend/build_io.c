#include "build_io.h"

void _link_composon(const W* composon);
void _link_pair(const W* input, const W* output);
Ws* _recurse_tail(const W*);
Ws* _recurse_head(const W*);

void link_inputs(Ws* ws){

    // gather all composons
    Ws* cs = ws_rfilter(ws, ws_recurse_most, w_is_composon);

    // pass each into _link_one
    ws_mod(cs, _link_composon);

}

void _link_composon(const W* w){

    w_assert_class(w, C_COMPOSON);

    // identify the elements within this composon which take input
    Ws* inputs = composon_inputs(w);

    // identify the elements in the next composon which produce output
    Ws* outputs = composon_outputs(w->next);

    // link each input to each output
    ws_2mod(inputs, outputs, _link_pair);
}

Ws* composon_inputs(const W* w){

    if(!w) return NULL;

    w_assert_class(w, C_COMPOSON);

    return ws_rfilter(g_ws(w), _recurse_tail, w_is_manifold);
}

Ws* composon_outputs(const W* w){

    if(!w) return NULL;

    w_assert_class(w, C_COMPOSON);

    return ws_rfilter(g_ws(w), _recurse_head, w_is_manifold);
}

// link all top level elements in c_{i+1} as inputs to c_i
void _link_pair(const W* input, const W* output){
    if(!input || !output) return;
    // I'm dealing with C_MANIFOLD's not P_MANIFOLD's
    // A C_MANIFOLD manifold is a couplet with a P_MANIFOLD rhs
    if(input->cls == C_MANIFOLD){
        Manifold* m = g_manifold(g_rhs(input));
        m->inputs = ws_add(m->inputs, output);
    }
}

Ws* _recurse_tail(const W* w){
    Ws* result = NULL;
    switch(w->cls){
        case C_NEST:
            result = ws_add_val(result, V_WS, g_ws(g_ws(w)->tail));
            break;
        case T_PATH:
            result = ws_add_val(result, V_WS, g_ws(g_ws(g_rhs(w))->tail));
            break;
        default:
            break;
    }
    return result;
}

Ws* _recurse_head(const W* w){
    Ws* result = NULL;
    switch(w->cls){
        case C_NEST:
            result = ws_add_val(result, V_WS, g_ws(g_ws(w)->head));
            break;
        case T_PATH:
            result = ws_add_val(result, V_WS, g_ws(g_ws(g_rhs(w))->head));
            break;
        default:
            break;
    }
    if(ws_length(result) > 1){
        fprintf(stderr, "A nested expression MUST have exactly 1 output.\n");
    }
    return result;
}
