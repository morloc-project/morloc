#include "type.h"

bool _is_io(W* w){
    return strcmp(g_string(w), __IO__) == 0;
}

void set_default_types(const Ws* ws){
    /* STUB */
}

Ws* infertype(const Ws* ws){
    /* STUB */
    return NULL;
}

void _typecheck(W* w){
    Manifold* m = g_manifold(g_rhs(w));

    if(!m->type){
        fprintf(
            stderr,
            "WARNING: '%s' has no delared type\n",
            m->function
        );
        return;
    }

    int n_types = ws_length(m->type) - 1 - type_is_well(m->type);
    int n_inputs = ws_length(m->inputs);

    if(ws_length(m->type) < 2){
        fprintf(
            stderr,
            "ERROR: '%s' type length < 2\n",
            m->function
        ); 
    }

    if(n_inputs < n_types){
        fprintf(
            stderr,
            "ERROR: '%s' is not closed\n",
            m->function
        );
    }

    if(n_inputs > n_types){
        fprintf(
            stderr,
            "ERROR: '%s' expects %d inputs, gets %d\n",
            m->function, n_types, n_inputs
        );
    }
}

void type_check(const Ws* ws){
    ws_rcmod(ws, ws_recurse_composition, w_is_manifold, _typecheck);
}

bool type_is_well(const Ws* type){
    return _is_io(type->head) && !_is_io(type->tail);
}

bool type_is_pipe(const Ws* type){
    return !_is_io(type->head) && !_is_io(type->tail);
}

bool type_is_sink(const Ws* type){
    return !_is_io(type->head) && _is_io(type->tail);
}
