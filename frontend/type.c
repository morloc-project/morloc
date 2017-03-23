#include "type.h"

#define IS_ATOMIC(t) ((t)->cls == FT_ATOMIC)
#define IS_GENERIC(t) ((t)->cls == FT_GENERIC)
#define IS_ARRAY(t) ((t)->cls == FT_ARRAY)
#define IS_STAR(t) (strcmp(g_string((t)), __WILD__) == 0)
#define IS_MULTI(t) (strcmp(g_string((t)), __MULTI__) == 0)

void _set_default_type(W* w);

void _infer_multi_type(W* w);
void _infer_generic_type(W* w);
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

#define CHK(x)                                              \
if((p + x) >= MAX_TYPE_LENGTH) {                            \
    warn("Type buffer exceeded, truncating type string\n"); \
    return p;                                               \
}

    switch(w->cls){
        case FT_FUNCTION:
        {
            int i = 0;
            CHK(1)
            s[p++] = '(';
            for(W* wt = wws_head(w); wt != NULL; wt = wt->next){
                if(i > 0){
                    CHK(2)
                    s[p++] = '-';
                    s[p++] = '>';
                }
                p = type_str_r(wt, s, p);
                i++;
            }
            CHK(1)
            s[p++] = ')';
        }
            break;
        case FT_TUPLE:
        {
            int i = 0;
            s[p++] = '(';
            for(W* wt = wws_head(w); wt != NULL; wt = wt->next){
                if(i > 0){
                    CHK(1)
                    s[p++] = ',';
                }
                p = type_str_r(wt, s, p);
                i++;
            }
            s[p++] = ')';
        }
            break;
        case FT_ARRAY:
        {
            CHK(1)
            s[p++] = '[';
            p = type_str_r(wws_head(w), s, p);
            CHK(1)
            s[p++] = ']';
        }
            break;
        case FT_GENERIC:
            // - The lhs holds the generic label (e.g. 'a')
            // - The rhs of a generic holds the inferred type it will be of type
            // FT_*, so can be thrown into the next cycle
            p = type_str_r(g_rhs(w), s, p);
            break;
        case FT_ATOMIC:
        {
            char* atom = g_string(w);
            int atom_size = strlen(atom);
            CHK(atom_size)
            strcpy(s + p, atom);
            p += atom_size;
            break;
        }
        default:
            warn("Unusual error (%s:%d)", __func__, __LINE__); 
            break;
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
    return
        w->cls == FT_ATOMIC &&
        strcmp(g_string(w), __IO__) == 0;
}

bool _cmp_type(char* a, char* b){
    return
           ( strcmp(a,  b ) == 0 ) ||
           ( strcmp(a, __WILD__) == 0 ) ||
           ( strcmp(b, __WILD__) == 0 );
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
            W* star;
            if(i == 0 && ninputs == 0){
                star = w_new(FT_ATOMIC, __IO__);
            } else {
                star = w_new(FT_ATOMIC, __WILD__);
            }
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
        m->type,
        m->inputs,
        _transfer_star_type
    );
}
void _transfer_star_type(W* type, W* input){
    if(input->cls == C_MANIFOLD){
        W* itype = g_manifold(g_rhs(input))->type->last;
        if(
                IS_ATOMIC(type) && IS_STAR(type) &&
                ! (IS_ATOMIC(itype) && ( IS_STAR(itype) || IS_MULTI(itype)))
          ){
            type->value = itype->value;
            type->cls = itype->cls;
        }
        if(
                IS_ATOMIC(itype) && IS_STAR(itype) &&
                ! (IS_ATOMIC(type) && ( IS_STAR(type) || IS_MULTI(type)))
          ){
            itype->value = type->value;
            itype->cls = type->cls;
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
       wm->type->head->cls == FT_ATOMIC &&
       strcmp(g_string(wm->type->head), __MULTI__) == 0 &&
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
                        char* ptype = g_string(g_lhs(i));
                        wm->type = ws_add(wm->type, w_new(FT_ATOMIC, ptype));
                    }
                    break;
                case C_MANIFOLD:
                    {
                        im = g_manifold(g_rhs(i));
                        if(ws_length(im->type) > 1){
                            wm->type = ws_add(wm->type, w_clone(im->type->last));
                        } else {
                            wm->type = ws_add(wm->type, w_new(FT_ATOMIC, __WILD__));
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

void infer_generic_types(Ws* ws){
    ws_rcmod(
        ws,
        ws_recurse_most,
        w_is_manifold,
        _infer_generic_type
    );
}
// TODO This stub function simply reduces generics to star types
//      I need to implement real handling
void _infer_generic_type(W* w){
    if(w->cls == FT_GENERIC){
        force_set_string(w, FT_ATOMIC, __WILD__);
    }
}

W* _typecheck(W* w, W* msg){
    Manifold* m = g_manifold(g_rhs(w));

    if(
        ws_length(m->type) == 2 &&
        m->type->head->cls == FT_ATOMIC &&
        strcmp(g_string(m->type->head), __MULTI__) == 0
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
