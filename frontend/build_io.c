#include "build_io.h"

void _link_composon(W* a, W* b);
void _link_pair(W* input, W* output);
Ws* _recurse_tail(W*);
Ws* _recurse_head(W*);
bool _is_emmisive(W*);
Ws* _extract_ws(W* w);

void _set_as_function(W* w);

void set_as_function(Ws* ws){
    ws_rcmod(
        ws,
        ws_recurse_most,
        w_is_deref,
        _set_as_function
    );
}

void _set_as_function(W* w){

    Ws* outputs = composon_outputs(w_new(C_COMPOSON, ws_new(w)));

    if(ws_length(outputs) > 1){
        warn("A functionalized composition can have only one output\n");
    }

    W* o = outputs->head;

    if(o->cls == C_MANIFOLD){
        Manifold* output = g_manifold(g_rhs(o));
        output->as_function = (w->cls == C_DEREF) ? true : false;
    }
}

void link_inputs(Ws* ws){

    ws_recursive_reduce_mod(
        ws,                     // recurse over full symbol table
        ws_recurse_composition, // descend into each composition
        w_is_composon,          // assert e_i must take input
        w_is_composon,          // assert e_{i+1} must produce output
        _link_composon          // link output of b to input of a
    );

}


void _link_composon(W* a, W* b){

    // identify the elements within this composon which take input
    Ws* inputs = composon_inputs(a);

    // identify the elements in the next composon which produce output
    Ws* outputs = composon_outputs(b);

    // link each input to each output
    ws_2mod(inputs, outputs, _link_pair);
}

Ws* composon_inputs(W* w){
    // recurse to the rightmost manifold set
    return ws_rfilter(_extract_ws(w), _recurse_tail, w_is_manifold);
}
Ws* composon_outputs(W* w){
    // recurse to the leftmost manifold set
    return ws_rfilter(_extract_ws(w), _recurse_head, _is_emmisive);
}

Ws* _extract_ws(W* w){
    if(!w) return NULL;
    return w->cls == T_PATH ? g_ws(g_rhs(w)): g_ws(w);
}

bool _is_emmisive(W* w){
    switch(w->cls){
        case C_MANIFOLD:
        case C_POSITIONAL:
        case C_ARGREF:
        case C_REFER:
            return true;
        default:
            return false;
    }
}

Ws* _recurse_tail(W* w){
    Ws* result = NULL;
    switch(w->cls){
        case C_NEST:
            result = ws_add_val(result, V_WS, g_ws(g_ws(w)->last));
            break;
        case T_PATH:
            result = ws_add_val(result, V_WS, g_ws(g_ws(g_rhs(w))->last));
            break;
        default:
            break;
    }
    return result;
}

Ws* _recurse_head(W* w){
    Ws* result = NULL;
    switch(w->cls){
        case C_NEST:
        case C_DEREF:
            result = ws_add_val(result, V_WS, g_ws(g_ws(w)->head));
            break;
        case T_PATH:
            result = ws_add_val(result, V_WS, g_ws(g_ws(g_rhs(w))->head));
            break;
        default:
            break;
    }
    if(ws_length(result) > 1){
        warn("A nested expression MUST have exactly 1 output.\n");
    }
    return result;
}

// link all top level elements in c_{i+1} as inputs to c_i
void _link_pair(W* input, W* output){
    if(!input || !output) return;
    // I'm dealing with C_MANIFOLD's not P_MANIFOLD's
    // A C_MANIFOLD manifold is a couplet with a P_MANIFOLD rhs
    if(input->cls == C_MANIFOLD){
        Manifold* m = g_manifold(g_rhs(input));
        m->inputs = ws_add(m->inputs, output);
        // Propagate number of manifold arguments to inputs
        if(m->nargs != 0 && output->cls == C_MANIFOLD){
           Manifold* o = g_manifold(g_rhs(output)); 
           o->nargs = m->nargs;
        }
    }
}
