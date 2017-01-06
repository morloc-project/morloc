#include "type.h"

void _type_compatible(const W* i, const W* t);

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

    if(n_inputs && n_inputs < n_types){
        fprintf(
            stderr,
            "UNSUPPORTED: '%s' is underspecified; I do not support currying.\n",
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

    Ws* itypes;
    if(type_is_well(m->type)){
        itypes = NULL;
        return;
    } else {
        itypes = ws_init(m->type);
    }
    ws_zip_mod(m->inputs, itypes, _type_compatible);

}

void type_check(const Ws* ws){
    ws_rcmod(ws, ws_recurse_composition, w_is_manifold, _typecheck);
}

void _type_compatible(const W* o, const W* t){
    if(o->cls == C_DEREF || o->cls == C_POSITIONAL){
        /* I currently do no type checking on these */
        return;
    }
    Manifold *m = g_manifold(g_rhs(o));
    if(!m->type){
        fprintf(
            stderr,
            "WARNING: Output of '%s' cannot be checked since it is typeless\n",
            m->function
        );
    } else {
        char* o_type = g_string(m->type->last);
        char* i_type = g_string(t); 
        if(strcmp(o_type, i_type) != 0){
            fprintf(
                stderr,
                "ERROR: output of '%s' incomatible with input\n",
                m->function
            );
        }
    }
}

bool type_is_well(const Ws* type){
    return _is_io(type->head) && !_is_io(type->last);
}

bool type_is_pipe(const Ws* type){
    return !_is_io(type->head) && !_is_io(type->last);
}

bool type_is_sink(const Ws* type){
    return !_is_io(type->head) && _is_io(type->last);
}
