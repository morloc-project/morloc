#include "build_mod.h"

void _set_manifold_defaults(W* cm);
void _set_manifold_type(W*, W*);
void _transfer_type(W* type_w, W* man_w);
bool _manifold_modifier(W* w);
void _mod_add_modifiers(Ws* ws_top, W* p);
bool _basename_match(W* w, W* p);
void _add_modifier(W* w, W* p);
Ws*  _do_operation(Ws* ws, W* p, char op);

void link_modifiers(Ws* ws_top){

    // Set default function names for all manifolds
    ws_filter_mod(ws_top, get_manifolds, _set_manifold_defaults);

    // Set manifold type based off the manifold names
    ws_2mod(
        // get all manifolds
        ws_rfilter(ws_top, ws_recurse_composition, w_is_manifold),
        // get all defined types
        ws_rfilter(ws_top, ws_recurse_section, w_is_type),
        // if the names match, add the type to the manifold
        _set_manifold_type
    );

    // set manifold languages
    // this must be done prior to setting other modifiers
    Ws* langs = ws_rfilter(ws_top, ws_recurse_most, w_is_lang);
    langs = ws_map_split(langs, ws_split_couplet);
    ws_map_pmod(ws_top, langs, _mod_add_modifiers);

    // add modifiers to all manifolds
    Ws* cs = ws_rfilter(ws_top, ws_recurse_most, _manifold_modifier);
    cs = ws_map_split(cs, ws_split_couplet);
    ws_map_pmod(ws_top, cs, _mod_add_modifiers);

}

// Given the couplet {Label, Manifold}, transfer the name from Label to
// Manifold->function IFF it is not already defined.
void _set_manifold_defaults(W* cm){
    Manifold* m = g_manifold(g_rhs(cm));
    char* name = g_label(g_lhs(cm))->name;
    char* lang = g_label(g_lhs(cm))->lang;
    m->function = strdup(name);
    m->lang = lang ? strdup(lang) : NULL;
}

void _set_manifold_type(W* mw, W* tw){

    Ws* w_types = ws_split_couplet(tw);

    ws_cap(
        w_types,         // for t in types
        mw,              //   given manifold m
        w_string_equal,  //   if name(m) == name(type)
        _transfer_type   //     m->type = type
    );
}
void _transfer_type(W* type_w, W* man_w){
    Manifold* m = g_manifold(g_rhs(man_w));
    if(m->type){
        fprintf(stderr, "TYPE WARNING: redeclaration of '%s' type\n", m->function);
    }
    m->type = g_ws(g_rhs(type_w));
}

bool _manifold_modifier(W* w){
    switch(w->cls){
        case T_H0:
        case T_H1:
        case T_H2:
        case T_H3:
        case T_H4:
        case T_H5:
        case T_H6:
        case T_H7:
        case T_H8:
        case T_H9:
        case T_CACHE:
        case T_CHECK:
        case T_FAIL:
        case T_ALIAS:
        case T_DOC:
        case T_ARGUMENT:
            return true;         
        case T_LANG: // Language must be set first
            return false;
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

char* get_lang(W* w){
    W* lhs = g_lhs(w);
    char* lang = NULL;
    switch(lhs->cls){
        case K_LABEL:
            lang = g_label(lhs)->lang; 
            break;
        case K_LIST:
        case K_PATH:
            lang = g_label(g_ws(lhs)->head)->lang; 
            break;
        case K_NAME:
            lang = NULL; 
            break;
        default:
            lang = NULL; 
            break;
    }
    return lang;
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

    char* mod_lang = get_lang(p);
    char* man_lang = m->lang;

    if(mod_lang &&
       man_lang &&
       strcmp(mod_lang, man_lang) != 0) return;

    switch(p->cls){
        case T_ALIAS:
            if(g_string(rhs)){
                m->function = g_string(rhs);
            } else {
                _set_manifold_defaults(w);
            }
            break;
        case T_LANG:
            m->lang = g_string(rhs) ? g_string(rhs) : "*";
            break;

        /* For compositional modifiers add all ultimate manifolds */
        case T_H0: m->h0 = g_ws(rhs) ? _do_operation(m->h0, g_ws(rhs)->head, op) : NULL; break; 
        case T_H1: m->h1 = g_ws(rhs) ? _do_operation(m->h1, g_ws(rhs)->head, op) : NULL; break;
        case T_H2: m->h2 = g_ws(rhs) ? _do_operation(m->h2, g_ws(rhs)->head, op) : NULL; break;
        case T_H3: m->h3 = g_ws(rhs) ? _do_operation(m->h3, g_ws(rhs)->head, op) : NULL; break;
        case T_H4: m->h4 = g_ws(rhs) ? _do_operation(m->h4, g_ws(rhs)->head, op) : NULL; break;
        case T_H5: m->h5 = g_ws(rhs) ? _do_operation(m->h5, g_ws(rhs)->head, op) : NULL; break;
        case T_H6: m->h6 = g_ws(rhs) ? _do_operation(m->h6, g_ws(rhs)->head, op) : NULL; break;
        case T_H7: m->h7 = g_ws(rhs) ? _do_operation(m->h7, g_ws(rhs)->head, op) : NULL; break;
        case T_H8: m->h8 = g_ws(rhs) ? _do_operation(m->h8, g_ws(rhs)->head, op) : NULL; break;
        case T_H9: m->h9 = g_ws(rhs) ? _do_operation(m->h9, g_ws(rhs)->head, op) : NULL; break;

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
