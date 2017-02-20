#include "type.h"

#define IS_ATOMIC(t) (strcmp(g_string(g_lhs((t))), "atomic") == 0)
#define IS_ARRAY(t) (strcmp(g_string(g_lhs((t))), "array") == 0)
#define IS_STAR(t) (strcmp(g_string(g_rhs((t))), "*") == 0)

void _set_default_type(W* w);

void _infer_multi_type(W* w);

void _infer_star_type(W* w);
void _transfer_star_type(W* type, W* input);

// takes in all data
W* _typecheck_derefs(Ws* ws_top, W* msg);

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
    ws_rcmod(
        ws,
        ws_recurse_most,
        w_is_manifold,
        _set_default_type
    );
}
void _set_default_type(W* w){
    Manifold* m = g_manifold(g_rhs(w));
    if(!m->type){
        int ninputs = ws_length(m->inputs);
        int ntypes = ninputs ? ninputs + 1 : 2;
        for(int i = 0; i < ntypes; i++){
            W *lhs,*rhs;
            lhs = w_new(P_STRING, "atomic");
            if(i == 0 && ninputs == 0){
                rhs = w_new(P_STRING, "void");
            } else {
                rhs = w_new(P_STRING, "*");
            }
            Couplet* c = couplet_new(lhs, rhs, '=');
            W* star = w_new(P_TYPE, c);
            m->type = ws_add(m->type, star);
        } 
    }
}

void infer_star_types(Ws* ws){
    ws_rcmod(
        ws,
        ws_recurse_most,
        w_is_manifold,
        _infer_star_type
    );
}
void _infer_star_type(W* w){
    Manifold* m = g_manifold(g_rhs(w));
    ws_zip_mod(
        ws_init(m->type),
        m->inputs,
        _transfer_star_type
    );
}
void _transfer_star_type(W* type, W* input){
    if(input->cls == C_MANIFOLD){
        W* itype = g_manifold(g_rhs(input))->type->last;
        if(IS_ATOMIC(type) && IS_STAR(type)){
            s_lhs(type, g_lhs(itype));
            s_rhs(type, g_rhs(itype));
        }
    }
}


void infer_multi_types(Ws* ws){
    ws_rcmod(
        ws,
        ws_recurse_most,
        w_is_manifold,
        _infer_multi_type
    );
}
void _infer_multi_type(W* w){
    Manifold *wm, *im;
    wm = g_manifold(g_rhs(w));
    if(wm->type &&
       strcmp(g_string(g_lhs(wm->type->head)), "atomic") == 0 &&
       strcmp(g_string(g_rhs(wm->type->head)), __MULTI__) == 0 &&
       ws_length(wm->type) == 2 &&
       ws_length(wm->inputs) > 1
    ){
        W* output = w_isolate(wm->type->last); 
        wm->type = NULL;
        for(W* i = wm->inputs->head; i != NULL; i = i->next){
            switch(i->cls){
                case C_ARGREF:
                    break;
                case C_POSITIONAL:
                    {
                        W* lhs = w_new(P_STRING, "atomic");
                        W* rhs = w_new(P_STRING, g_lhs(i));
                        Couplet* c = couplet_new(lhs, rhs, '=');
                        wm->type = ws_add(wm->type, w_new(P_TYPE, c));
                    }
                    break;
                case C_MANIFOLD:
                    {
                        im = g_manifold(g_rhs(i));
                        if(ws_length(im->type) > 1){
                            wm->type = ws_add(wm->type, w_clone(im->type->last));
                        } else {
                            W* lhs = w_new(P_STRING, "atomic");
                            W* rhs = w_new(P_STRING, "*");
                            Couplet* c = couplet_new(lhs, rhs, '=');
                            wm->type = ws_add(wm->type, w_new(P_TYPE, c));
                        }
                    }
                    break;
                default:
                    warn("Unexpected input type (%s:%d)\n", __func__, __LINE__);
            }
        }
        wm->type = ws_add(wm->type, output);
    }
}

W* _typecheck(W* w, W* msg){
    Manifold* m = g_manifold(g_rhs(w));

    if(
        ws_length(m->type) == 2 &&
        strcmp(g_string(g_lhs(m->type->head)), "atomic") == 0 &&
        strcmp(g_string(g_rhs(m->type->head)), __MULTI__) == 0
    ){
        return msg;
    }

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

W* type_check(Ws* ws){
    W* w = ws_scrap(ws, NULL, ws_recurse_composition, w_is_manifold, _typecheck);
    w = _typecheck_derefs(ws, w);
    return w;
}

W* _typecheck_derefs(Ws* ws, W* msg){
    /* STUB */
    return msg;
}

W* _type_compatible(W* o, W* t, W* msg){
    switch(o->cls){
        case C_DEREF:
        case C_REFER:
        case C_ARGREF:
            /* I currently do no type checking on these */
            break;
        case C_MANIFOLD:
        {
            Manifold *m = g_manifold(g_rhs(o));
            if(!m->type){
                LOG_ERROR(msg, o, "cannot check usage of untyped output");
            }
            else if(!m->as_function){
                char* o_type = type_str(m->type->last);
                char* i_type = type_str(t); 
                if( ! _cmp_type(o_type, i_type)){
                    char* fmt = "type conflict '%s' vs '%s'\n";
                    int size =
                        strlen(fmt)    - // length of format string
                        4              + // subtract the '%s'
                        strlen(o_type) + // string lengths
                        strlen(i_type) + // ''
                        1;               // add 1 for \0
                    char* errmsg = (char*)malloc(size * sizeof(char));
                    sprintf(errmsg, fmt, o_type, i_type);
                    LOG_ERROR(msg, o, errmsg);
                }
            }
        }
            break;
        case C_POSITIONAL:
        {
            char* o_type = g_string(g_lhs(o));
            char* i_type = type_str(t);
            if( ! _cmp_type(o_type, i_type)){
                char* fmt = "type conflict positional ('%s') '%s' vs '%s'\n";
                int size =
                    strlen(fmt)                - // length of the format string
                    6                          + // subtract the '%s'
                    strlen(o_type)             + // add length of type string
                    strlen(g_string(g_rhs(o))) + // ''
                    strlen(i_type)             + // ''
                    1;                           // add 1 for \0
                char* errmsg = (char*)malloc(size * sizeof(char));
                sprintf(errmsg, fmt, o_type, g_string(g_rhs(o)), i_type);
                LOG_ERROR(msg, o, errmsg);
            }
        }
            break;
        default: 
            break;
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
        switch(g_lhs(w)->cls){
            case C_MANIFOLD:
            {
                warn(
                    "TYPE ERROR in %s: %s\n",
                    g_manifold(g_rhs(g_lhs(w)))->function,
                    g_string(g_rhs(w))
                );
            }
                break;
            case C_POSITIONAL:
            {
                warn(
                    "TYPE ERROR: positional is of type %s, but got %s\n",
                    g_string(g_lhs(g_lhs(w))),
                    g_string(g_rhs(w))
                );
            }
            default:
                break;
        }
    }
}
