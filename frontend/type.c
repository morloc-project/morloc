#include "type.h"

#define LOG_ERROR(st, w, s)                                 \
    do{                                                     \
        W* wstr = w_new(P_STRING, strdup(s));               \
        Couplet* cerr = couplet_new(w_clone(w), wstr, '='); \
        W* werr = w_new(P_COUPLET, cerr);                   \
        if(st){                                             \
            s_ws(st, ws_add(g_ws(st), werr));               \
        } else {                                            \
            Ws* wserr = ws_new(werr);                       \
            st = w_new(P_WS, wserr);                        \
        }                                                   \
    } while(0)

int type_str_r(W* w, char* s, int p){
#define CHK(x) \
if((p + x) >= MAX_TYPE_LENGTH) { \
    warn("Type buffer exceeded, truncating type string\n"); \
    return p; \
}
    if(w->cls == P_WS){
        int i = 0;
        for(W* wt = wws_head(w); wt != NULL; wt = wt->next){
            if(i > 0){
                CHK(2)
                s[p++] = '-';
                s[p++] = '>';
            }
            p = type_str_r(wt, s, p);
            i++;
        }
    } else {
        char* t = g_string(g_lhs(w));
        if(strcmp(t, "function") == 0){
            CHK(1)
            s[p++] = '(';
            p = type_str_r(g_rhs(w), s, p);
            CHK(1)
            s[p++] = ')';
        }
        else if(strcmp(t, "array") == 0){
            CHK(1)
            s[p++] = '[';
            p = type_str_r(g_rhs(w), s, p);
            CHK(1)
            s[p++] = ']';
        }
        else if(strcmp(t, "atomic") == 0){
            char* atom = g_string(g_rhs(w));
            int atom_size = strlen(atom);
            CHK(atom_size)
            strcpy(s + p, atom);
            p += atom_size;
        }
        else {
            warn("Type constructor '%s' is not supported", t); 
        }
    }
    return p;
#undef CHK
}

char* type_str(W* w){
   char* s = (char*)malloc(MAX_TYPE_LENGTH * sizeof(char));
   int p = type_str_r(w, s, 0);
   s[p] = '\0';
   char* ss = strdup(s);
   free(s);
   return ss;
}

W* _type_compatible(W* i, W* t, W* msg);

bool _is_io(W* w){
    return strcmp(g_string(g_lhs(w)), __IO__) == 0;
}

bool _cmp_type(char* a, char* b){
    return
           ( strcmp(a,  b ) == 0 ) ||
           ( strcmp(a, "*") == 0 ) ||
           ( strcmp(b, "*") == 0 );
}

void set_default_types(Ws* ws){
    /* STUB */
}

Ws* infertype(Ws* ws){
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

    /* Ws* itypes;                                              */
    /* if(type_is_well(m->type)){                               */
    /*     itypes = NULL;                                       */
    /*     return msg;                                          */
    /* } else {                                                 */
    /*     itypes = ws_init(m->type);                           */
    /* }                                                        */
    /* msg = ws_szap(m->inputs, itypes, msg, _type_compatible); */

    return msg;
}

W* type_check(Ws* ws){
    return ws_scrap(ws, NULL, ws_recurse_composition, w_is_manifold, _typecheck);
}

W* _type_compatible(W* o, W* t, W* msg){
    if(o->cls == C_DEREF || o->cls == C_POSITIONAL || o->cls == C_ARGREF){
        /* I currently do no type checking on these */
        return msg;
    }
    Manifold *m = g_manifold(g_rhs(o));
    if(!m->type){
        LOG_ERROR(msg, o, "cannot check usage of untyped output");
    } else {
        char* o_type = g_string(m->type->last);
        char* i_type = g_string(t); 
        if( ! _cmp_type(o_type, i_type)){
            LOG_ERROR(msg, o, "type conflict with calling manifold");
        }
    }
    return msg;
}

bool type_is_well(Ws* type){
    return _is_io(type->head) && !_is_io(type->last);
}

bool type_is_pipe(Ws* type){
    return !_is_io(type->head) && !_is_io(type->last);
}

bool type_is_sink(Ws* type){
    return !_is_io(type->head) && _is_io(type->last);
}

void print_error(W* msg){
    if(!msg) return;
    for(W* w = g_ws(msg)->head; w; w = w->next){
        warn(
            "TYPE ERROR in %s: %s\n",
            g_manifold(g_rhs(g_lhs(w)))->function,
            g_string(g_rhs(w))
        );
    }
}
