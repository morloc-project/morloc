#include "build_mod.h"

void _set_default_manifold_function(W* cm);
void _set_manifold_type(W*, W*);
bool _manifold_modifier(W* w);
void _mod_add_modifiers(Ws* ws_top, W* p);
bool _basename_match(W* w, W* p);
void _add_modifier(W* w, W* p);
Ws*  _do_operation(Ws* ws, W* p, char op);



void link_modifiers(Ws* ws_top){

    // Set default function names for all manifolds
    ws_filter_mod(ws_top, get_manifolds, _set_default_manifold_function);

    // Set manifold type based off the default names
    ws_2mod(
        // get all manifolds
        ws_rfilter(ws_top, ws_recurse_composition, w_is_manifold),
        // get all defined types
        ws_rfilter(ws_top, ws_recurse_none, w_is_type),
        // if the names match, add the type to the manifold
        _set_manifold_type
    );

    // add modifiers to all manifolds
    Ws* cs = ws_rfilter(ws_top, ws_recurse_most, _manifold_modifier);
    cs = ws_map_split(cs, ws_split_couplet);
    ws_map_pmod(ws_top, cs, _mod_add_modifiers);

}

// Given the couplet {Label, Manifold}, transfer the name from Label to
// Manifold->function IFF it is not already defined.
void _set_default_manifold_function(W* cm){
    Manifold* m = g_manifold(g_rhs(cm));
    m->function = strdup(g_label(g_lhs(cm))->name);
}

void _set_manifold_type(W* mw, W* tw){
    char* m_name = g_label(g_lhs(mw))->name;
    char* t_name = g_string(g_lhs(tw));
    if(strcmp(m_name, t_name) == 0){
        Manifold* m = g_manifold(g_rhs(mw));
        if(m->type){
            warn("TYPE ERROR: redeclarations of '%s' type", m_name);
        } else {
            m->type = g_ws(g_rhs(tw));
        }
    }
}

bool _manifold_modifier(W* w){
    switch(w->cls){
        case T_EFFECT:
        case T_HOOK:
        case T_CACHE:
        case T_CHECK:
        case T_OPEN:
        case T_PACK:
        case T_PASS:
        case T_FAIL:
        case T_ALIAS:
        case T_LANG:
        case T_DOC:
        case T_ARGUMENT:
            return true;         
        default:
            return false;
    } 
}

void _mod_add_modifiers(Ws* ws_top, W* p){
    ws_prmod(
        ws_top,
        p,
        ws_recurse_path,
        _basename_match,
        _add_modifier,
        w_nextval_ifpath
    );
}

bool _basename_match(W* w, W* p){
    bool result = false;
    if(w->cls == C_MANIFOLD){
        Ws* pws = g_ws(g_lhs(p));
        result =
            ws_length(pws) == 1 &&
            label_cmp(g_label(pws->head), g_label(g_lhs(w)));
    }
    return result;
}

// add the modifier stored in p (rhs of couplet) to w
// if:
//   1. the p->lhs contains only one name
//   2. the name matches the name of w
void _add_modifier(W* w, W* p){
    if(!p || w->cls != C_MANIFOLD) return;
    Manifold* m = g_manifold(g_rhs(w));
    W* rhs = g_rhs(p);
    char op = g_couplet(p)->op;

    switch(p->cls){
        case T_ALIAS:
            if(g_string(rhs)){
                m->function = g_string(rhs);
            } else {
                _set_default_manifold_function(w);
            }
            break;
        case T_LANG:
            m->lang = g_string(rhs) ? g_string(rhs) : "*";
            break;

        /* For compositional modifiers add all ultimate manifolds */
        case T_EFFECT:
            m->effect = g_ws(rhs) ? _do_operation( m->effect , g_ws(rhs)->head, op) : NULL;
            break;                                 
        case T_HOOK:                               
            m->hook   = g_ws(rhs) ? _do_operation( m->hook   , g_ws(rhs)->head, op) : NULL;
            break;                                 
        case T_CHECK:                              
            m->check  = g_ws(rhs) ? _do_operation( m->check  , g_ws(rhs)->head, op) : NULL;
            break;                                 
        case T_FAIL:                               
            m->fail   = g_ws(rhs) ? _do_operation( m->fail   , g_ws(rhs)->head, op) : NULL;
            break;

        case T_ARGUMENT:
            op = g_couplet(rhs) ? op : '!';
            switch(op){
                case '-':
                    warn(
                        "The ':-' operator is not supported for args."
                        " Nor will it ever be (%s:%d)\n",
                        __func__, __LINE__
                    );
                    break;
                case '=':
                    m->args = ws_new(rhs);
                    break;
                case '+':
                    m->args = ws_add_val(m->args, P_ARGUMENT, g_couplet(rhs));
                    break;
                case '!':
                    m->args = NULL;
                    break;
                default:
                    warn(
                        "Unexpected operator at (%s:%d)\n",
                        __func__, __LINE__
                    );
                    break;
            }
            break;

        case T_CACHE:
            m->cache = g_string(rhs) ? ws_add_val(m->cache, P_STRING, g_string(rhs)) : NULL;
            break;
        case T_OPEN:
            m->open = g_string(rhs) ? ws_add_val(m->open, P_STRING, g_string(rhs)) : NULL;
            break;
        case T_PACK:
            m->pack = g_string(rhs) ? ws_add_val(m->pack, P_STRING, g_string(rhs)) : NULL;
            break;
        case T_PASS:
            m->pass = g_string(rhs) ? ws_add_val(m->pass, P_STRING, g_string(rhs)) : NULL;
            break;
        case T_DOC:
            m->doc = g_string(rhs) ? ws_add_val(m->doc, P_STRING, g_string(rhs)) : NULL;
            break;
        default:
            break;
            warn(
                "Illegal p (%s) in %s:%d\n",
                w_class_str(p->cls), __func__, __LINE__
            );
    }
}

bool _none_match(W* w, W* ps){
    bool result = true;
    Manifold* mw = g_manifold(g_rhs(w));
    for(W* p = g_ws(ps)->head; p; p = p->next){
        if(mw->uid == g_manifold(g_rhs(p))->uid){
            result = false;
            break;
        }
    }
    return result;
}

Ws* _do_operation(Ws* ws, W* p, char op){
    switch(op){
        case '+':
            ws = ws_join(ws, g_ws(p));
            break;
        case '=':
            ws = ws_copy(g_ws(p));
            break;
        case '-':
            ws = ws_pfilter(ws, p, _none_match);
            break;
        default:
            warn(
                "Unexpected operator (%c) in %s:%d\n",
                op, __func__, __LINE__
            );
            break;
    }
    return ws;
}
