#include "lil.h"

char* _mid(Manifold* m);
char* _mid(Manifold* m);
char* _num(int i);
W* _wtwo(char* c1, char* c2);
W* _wthree(char* c1, char* c2, char* c3);
W* _wfour(char* c1, char* c2, char* c3, char* c4);
Ws* _two(Ws* ws, char* c1, char* c2);
Ws* _three(Ws* ws, char* c1, char* c2, char* c3);
Ws* _four(Ws* ws, char* c1, char* c2, char* c3, char* c4);
Ws* _pairlist(Ws* lil, Ws* ws, char* cmd, Manifold* m);
Ws* _pathmod(Ws* lil, Ws* ws, char* cmd, Manifold* m);
Ws* _manifold_to_lil(W* cm);
Ws* build_lil_prolog(Ws* ws_top);
Ws* build_lil_manifolds(Ws* ws_top);
Ws* build_lil_epilog(Ws* ws_top);

void print_lil(Ws* lil){
    if(!lil) return;
    for(W* w = lil->head; w; w = w->next){
        W* ww = wws_head(w);
        if(wws_length(w) > 3 && strcmp(LIL_SOURCE, g_string(ww)) == 0){
            printf(
                "%s\t%s\t%s\n",
                g_string(ww),
                g_string(ww->next),
                g_string(ww->next->next)
            );
            ww = ww->next->next->next;
            for(; ww; ww = ww->next){
                printf("    %s\n", g_string(ww));
            }
        } else {
            printf(g_string(ww));
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
        fprintf(stderr, "The symbol table is empty - nothing to do\n");
    }
    return lil;
}

Ws* build_lil_prolog(Ws* ws_top){
    Ws* lil = NULL;
    for(W* e = ws_top->head; e; e = e->next){
        if(e->cls == T_SOURCE){
            // I add an empty string at the initialization of each
            // source object, so if there is no code, the length will be 1,
            // not 0.
            if(ws_length(g_ws(g_rhs(e))) > 1){
                W* a = wws_join(
                    _wtwo(LIL_SOURCE, g_string(g_lhs(e))),
                    g_rhs(e)
                );
                lil = ws_add(lil, a);
            }
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
    for(W* e = ws_top->head; e; e = e->next){
        if(e->cls == T_EXPORT){
            Label* l = g_label(e);
            if(l->name){
                char* alias = l->label ? l->label : l->name;
                lil = _three(lil, LIL_EXPORT, l->name, alias);
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
        lil = _three(lil, LIL_TYPE, _mid(m), g_string(m->type->last));
    }

    if(m->nargs > 0){
        char* s = (char*)malloc(10 * sizeof(char));
        sprintf(s, "%d", m->nargs);
        lil = _three(lil, LIL_N_MANIFOLD_ARGS, _mid(m), s);
    }

    if(m->inputs){
        int i = 0;
        for(W* w = m->inputs->head; w; w = w->next){
            switch(w->cls){
                case C_REFER:
                case C_MANIFOLD:
                    lil = _four(
                        lil,
                        LIL_MANIFOLD_INPUT,
                        _mid(m),  // input id
                        _num(i++), // position
                        _mid(g_manifold(g_rhs(w))) // output id
                    );
                    break;
                case C_POSITIONAL:
                    lil = _four(
                        lil,
                        LIL_POSITIONAL_INPUT,
                        _mid(m),
                        _num(i++),
                        g_string(w)
                    );
                    break;
                case C_ARGREF:
                    lil = _four(
                        lil,
                        LIL_ARG_INPUT,
                        _mid(m),
                        _num(i++),
                        g_string(w)
                    );
                    break;
                case C_DEREF:
                    lil = _four(
                        lil,
                        LIL_FUNCTION_INPUT,
                        _mid(m),  // input id
                        _num(i++), // position
                        /*  // output id of sink of deref composition */
                    );
                    break;
                default:
                    break;
            }
        }
    }

    lil = _pathmod(lil, m->effect, LIL_EFFECT, m);
    lil = _pathmod(lil, m->hook,   LIL_HOOK,   m);
    lil = _pathmod(lil, m->check,  LIL_CHECK,  m);
    lil = _pathmod(lil, m->fail,   LIL_FAIL,   m);

    lil = _pairlist(lil, m->cache, LIL_CACHE,        m);
    lil = _pairlist(lil, m->open,  LIL_OPEN,         m);
    lil = _pairlist(lil, m->pack,  LIL_PACK,         m);
    lil = _pairlist(lil, m->pass,  LIL_PASS,         m);
    lil = _pairlist(lil, m->doc,   LIL_MANIFOLD_DOC, m);

    if(m->args){
        for(W* w = m->args->head; w; w = w->next){
            W* a = wws_join( 
                _wthree(LIL_FUNCTION_ARG, _mid(m), g_string(g_lhs(w))),
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
