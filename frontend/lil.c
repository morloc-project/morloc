#include "lil.h"

char* _mid(Manifold* m);
char* _mid(Manifold* m);
char* _num(int i);
W* _wtwo(char* c1, char* c2);
W* _wthree(char* c1, char* c2, char* c3);
W* _wfour(char* c1, char* c2, char* c3, char* c4);
W* _wfive(char* c1, char* c2, char* c3, char* c4, char* c5);
Ws* _two(Ws* ws, char* c1, char* c2);
Ws* _three(Ws* ws, char* c1, char* c2, char* c3);
Ws* _four(Ws* ws, char* c1, char* c2, char* c3, char* c4);
Ws* _five(Ws* ws, char* c1, char* c2, char* c3, char* c4, char* c5);
Ws* _pairlist(Ws* lil, Ws* ws, char* cmd, Manifold* m);
Ws* _pathmod(Ws* lil, Ws* ws, char* cmd, Manifold* m);
Ws* _manifold_to_lil(W* cm);
Ws* build_lil_prolog(Ws* ws_top);
Ws* build_lil_manifolds(Ws* ws_top);
Ws* build_lil_epilog(Ws* ws_top);
Ws* _hook(Ws* lil, Ws* ws, char* p, Manifold* m);

void print_lil(Ws* lil){
    if(!lil) return;
    for(W* w = lil->head; w; w = w->next){
        W* ww = wws_head(w);
        if(wws_length(w) > 2 && strcmp(LIL_SOURCE, g_string(ww)) == 0){
            printf(
                "%s\t%s\n",
                g_string(ww),
                g_string(ww->next)
            );
            ww = ww->next->next;
            for(; ww; ww = ww->next){
                // no newline needed, since newline from source is preserved
                printf("    %s", g_string(ww));
            }
        } else {
            printf("%s", g_string(ww));
            for(ww = ww->next; ww; ww = ww->next){
                printf("\t%s", g_string(ww));
            }
        }
        printf("\n");
    }
}

Ws* build_lil(Ws* ws_top){
    Ws* lil = NULL;
    if(ws_top && ws_top->head){
         lil = ws_join(lil, build_lil_prolog(ws_top));
         lil = ws_join(lil, build_lil_manifolds(ws_top));
         lil = ws_join(lil, build_lil_epilog(ws_top));
    } else {
        warn("The symbol table is empty - nothing to do\n");
    }
    return lil;
}

bool _is_source(W* w){
    return w->cls == T_SECTION &&
    g_lhs(w)->cls == P_SECTION &&
    strcmp(g_section(g_lhs(w))->name, "source") == 0;
}
Ws* build_lil_prolog(Ws* ws_top){
    Ws* lil = NULL;

    Ws* src = ws_rfilter(ws_top, ws_recurse_section, _is_source);

    if(!src) return lil;

    for(W* e = src->head; e; e = e->next){
        if(wws_length(g_rhs(e)) > 0){
            W* a = wws_join(
                _wtwo(LIL_SOURCE, g_section(g_lhs(e))->lang),
                g_rhs(e)
            );
            lil = ws_add(lil, a);
        }
    }
    return lil;
}

Ws* build_lil_manifolds(Ws* ws_top){
    Ws* ws_man = ws_rfilter(ws_top, ws_recurse_most, w_is_manifold);
    Ws* lil = ws_map_split(ws_man, _manifold_to_lil);  
    return lil;
}

Ws* build_lil_epilog(Ws* ws_top){
    Ws* lil = NULL;

    Ws* exp = ws_rfilter(ws_top, ws_recurse_section, w_is_export);

    if(!exp) return lil;

    for(W* e = exp->head; e; e = e->next){
        Ws* exports = get_by_name(ws_top, e);
        if(ws_length(exports) == 0){
            warn("Cannot export path\n");
        } else {
            Manifold* m = g_manifold(g_rhs(exports->head));
            char* alias = g_string(g_rhs(e));
            if(alias){
                lil = _three(lil, LIL_EXPORT, _mid(m), alias);
            } else {
                lil = _three(lil, LIL_EXPORT, _mid(m), m->function);
            }
        }
    }
    return lil;
}

Ws* _manifold_to_lil(W* cm){
    Manifold* m = g_manifold(g_rhs(cm));

    if(!m) return NULL;

    Ws* lil = NULL;

    lil = _three(lil, LIL_EMIT, _mid(m), m->lang ? m->lang : "*");

    lil = _three(lil, LIL_FUNCTION, _mid(m), m->function);

    if(m->type){
        char* s = type_str(m->type->last);
        lil = _three(lil, LIL_TYPE, _mid(m), s);
    }

    if(m->nargs > 0){
        char* s = (char*)malloc(10 * sizeof(char));
        sprintf(s, "%d", m->nargs);
        lil = _three(lil, LIL_N_MANIFOLD_ARGS, _mid(m), s);
    }

    if(m->inputs){
        bool is_typed = false;
        W* type = NULL;
        char* t_str = NULL;
        if(m->type && ws_length(m->type) == (ws_length(m->inputs) + 1)){
            is_typed = true;
            type = m->type->head;
        }
        int i = 0;
        for(W* w = m->inputs->head; w; w = w->next){
            t_str = is_typed ? type_str(type) : "*";
            if(is_typed)
                type = type->next;
            switch(w->cls){
                case C_REFER:
                case C_MANIFOLD:
                    if(g_manifold(g_rhs(w))->as_function) {
                        lil = _five(
                            lil,
                            LIL_FUNCTION_INPUT,
                            _mid(m),   // input id
                            _num(i++), // position
                            _mid(g_manifold(g_rhs(w))), // output id
                            t_str
                        );
                    } else {
                        lil = _five(
                            lil,
                            LIL_MANIFOLD_INPUT,
                            _mid(m),   // input id
                            _num(i++), // position
                            _mid(g_manifold(g_rhs(w))), // output id
                            t_str
                        );
                    }
                    break;
                case C_POSITIONAL:
                    lil = _five(
                        lil,
                        LIL_POSITIONAL_INPUT,
                        _mid(m),
                        _num(i++),
                        g_string(w),
                        t_str
                    );
                    break;
                case C_ARGREF:
                    lil = _five(
                        lil,
                        LIL_ARG_INPUT,
                        _mid(m),
                        _num(i++),
                        g_string(w),
                        t_str
                    );
                    break;
                case C_DEREF:
                    // build_io will have pointed the manifold to the
                    // appropriate manifold within the deref path
                    break;
                default:
                    break;
            }
        }
    }

    lil = _pathmod(lil, m->check,  LIL_CHECK,  m);
    lil = _pathmod(lil, m->fail,   LIL_FAIL,   m);

    lil = _hook(lil, m->h0,   "0",   m);
    lil = _hook(lil, m->h1,   "1",   m);
    lil = _hook(lil, m->h2,   "2",   m);
    lil = _hook(lil, m->h3,   "3",   m);
    lil = _hook(lil, m->h4,   "4",   m);
    lil = _hook(lil, m->h5,   "5",   m);
    lil = _hook(lil, m->h6,   "6",   m);
    lil = _hook(lil, m->h7,   "7",   m);
    lil = _hook(lil, m->h8,   "8",   m);
    lil = _hook(lil, m->h9,   "9",   m);

    lil = _pairlist(lil, m->cache, LIL_CACHE,        m);
    lil = _pairlist(lil, m->doc,   LIL_MANIFOLD_DOC, m);

    if(m->args){
        int i = 0;
        for(W* w = m->args->head; w; w = w->next){
            W* a = wws_join( 
                _wfour(LIL_FUNCTION_ARG, _mid(m), _num(i++), g_string(g_lhs(w))),
                g_rhs(w)
            );
            lil = ws_add(lil, a);
        }
    }

    return lil;
}

Ws* _pairlist(Ws* lil, Ws* ws, char* cmd, Manifold* m){
    if(!ws) return lil;
    for(W* w = ws->head; w; w = w->next){
        lil = _three(lil, cmd, _mid(m), g_string(w));
    }
    return lil;
}

Ws* _pathmod(Ws* lil, Ws* ws, char* cmd, Manifold* m){
    if(!ws) return lil;
    for(W* w = ws->head; w; w = w->next){
        lil = _three(lil, cmd, _mid(m), _mid(g_manifold(g_rhs(w))));
    }
    return lil;
}

Ws* _hook(Ws* lil, Ws* ws, char* p, Manifold* m){
    if(!ws) return lil;
    for(W* w = ws->head; w; w = w->next){
        lil = _four(lil, LIL_HOOK, _mid(m), p, _mid(g_manifold(g_rhs(w))));
    }
    return lil;
}

char* _mid(Manifold* m){
    char* s = (char*)calloc(20, sizeof(char));
    sprintf(s, "m%d", m->uid);
    return s;
}

char* _num(int i){
    char* s = (char*)calloc(20, sizeof(char));
    sprintf(s, "%d", i);
    return s;
}

W* _wtwo(char* c1, char* c2){
    W* a = w_new(P_STRING, c1);
    W* b = w_new(P_STRING, c2);
    W* out = wws_add(wws_new(a), b); 
    return out; 
}
W* _wthree(char* c1, char* c2, char* c3){
    return wws_add(_wtwo(c1, c2), w_new(P_STRING, c3)); 
}
W* _wfour(char* c1, char* c2, char* c3, char* c4){
    return wws_add(_wthree(c1, c2, c3), w_new(P_STRING, c4)); 
}
W* _wfive(char* c1, char* c2, char* c3, char* c4, char* c5){
    return wws_add(_wfour(c1, c2, c3, c4), w_new(P_STRING, c5)); 
}

// these just wrappers for the above three
Ws* _two(Ws* ws, char* c1, char* c2){
    return ws_add(ws, _wtwo(c1, c2));
}
Ws* _three(Ws* ws, char* c1, char* c2, char* c3){
    return ws_add(ws, _wthree(c1, c2, c3));
}
Ws* _four(Ws* ws, char* c1, char* c2, char* c3, char* c4){
    return ws_add(ws, _wfour(c1, c2, c3, c4));
}
Ws* _five(Ws* ws, char* c1, char* c2, char* c3, char* c4, char* c5){
    return ws_add(ws, _wfive(c1, c2, c3, c4, c5));
}
