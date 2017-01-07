#include "type.h"

#define LOG_ERROR(st, w, s)                            \
    do{                                                \
        W* wstr = w_new(P_STRING, strdup(s));          \
        Couplet* cerr = couplet_new(w_clone(w), wstr); \
        W* werr = w_new(P_COUPLET, cerr);              \
        if(st){                                        \
            s_ws(st, ws_add(g_ws(st), werr));          \
        } else {                                       \
            Ws* wserr = ws_new(werr);                  \
            st = w_new(P_WS, wserr);                   \
        }                                              \
    } while(0)

W* _type_compatible(const W* i, const W* t, W* msg);

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

W* _typecheck(W* w, W* msg){
    Manifold* m = g_manifold(g_rhs(w));

    if(!m->type){
        LOG_ERROR(msg, w, "no declared type");
        return msg;
    }

    int n_types = ws_length(m->type) - 1 - type_is_well(m->type);
    int n_inputs = ws_length(m->inputs);

    if(ws_length(m->type) < 2){
        LOG_ERROR(msg, w, "fewer than 2 terms in type");
    }

    if(n_inputs && n_inputs < n_types){
        LOG_ERROR(msg, w, "too few inputs (currying is not supported)");
    }

    if(n_inputs > n_types){
        LOG_ERROR(msg, w, "too many inputs");
    }

    Ws* itypes;
    if(type_is_well(m->type)){
        itypes = NULL;
        return msg;
    } else {
        itypes = ws_init(m->type);
    }
    msg = ws_szap(m->inputs, itypes, msg, _type_compatible);

    return msg;
}

W* type_check(const Ws* ws){
    return ws_scrap(ws, NULL, ws_recurse_composition, w_is_manifold, _typecheck);
}

W* _type_compatible(const W* o, const W* t, W* msg){
    if(o->cls == C_DEREF || o->cls == C_POSITIONAL){
        /* I currently do no type checking on these */
        return msg;
    }
    Manifold *m = g_manifold(g_rhs(o));
    if(!m->type){
        LOG_ERROR(msg, o, "cannot check usage of untyped output");
    } else {
        char* o_type = g_string(m->type->last);
        char* i_type = g_string(t); 
        if(strcmp(o_type, i_type) != 0){
            LOG_ERROR(msg, o, "type conflict with calling manifold");
        }
    }
    return msg;
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
