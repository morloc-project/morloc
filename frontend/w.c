#include "w.h"

VType get_value_type(Class cls){
    VType vtype;
    switch(cls){
        case C_COMPOSON:
        case C_NEST:
        case C_DEREF:
        case K_LIST:
        case K_PATH:
        case P_WS:
            vtype = V_WS;
            break;
        case C_POSITIONAL:
        case C_GRPREF:
        case P_STRING:
        case K_NAME:
            vtype = V_STRING;
            break;
        case T_PATH:
        case T_EFFECT:
        case T_CHECK:
        case T_OPEN:
        case T_PACK:
        case T_PASS:
        case T_FAIL:
        case T_DOC:
        case T_CACHE:
        case C_MANIFOLD:
        case P_COUPLET:
            vtype = V_COUPLET;
            break;
        case K_LABEL:
            vtype = V_LABEL;
            break;
        case P_MANIFOLD:
            vtype = V_MANIFOLD;
            break;
        case X_NONE:
            vtype = V_NONE;
            break;
        default:
            fprintf(stderr, "illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return vtype;
}

int _new_uid(){
    static int uid = 0;
    return uid++;
}

W* w_new(Class cls, void* value){
    W* w = (W*)calloc(1, sizeof(W));
    w->cls = cls;
    w->uid = _new_uid();
    w->next = NULL;

    switch(get_value_type(cls)){
        case V_NONE:
            w->value.none = NULL;
            break;
        case V_STRING:
            w->value.string = value;
            break;
        case V_WS:
            w->value.ws = value;
            break;
        case V_COUPLET:
            w->value.couplet = value;
            break;
        case V_LABEL:
            w->value.label = value;
            break;
        case V_MANIFOLD:
            w->value.manifold = value;
            break;
        default:
            fprintf(stderr, "illegal case (%s:%d)\n", __func__, __LINE__);
    }

    return w;
}

W* w_isolate(const W* w){
    W* new_w = w_copy(w);
    new_w->next = NULL;
    return new_w;
}

W* w_copy(const W* w){
   W* new_w = (W*)malloc(sizeof(W));
   memcpy(new_w, w, sizeof(W));
   return new_w;
}

W* w_clone(const W* w){
    if(!w) return NULL;
    W* clone = w_isolate(w);
    clone->uid = _new_uid();
    clone->next = w_clone(w->next);
    return clone;
}

char* w_type_str(VType type){
    char* s = NULL;
    switch(type){
        case V_NONE:     s = strdup("V_NONE");     break;
        case V_STRING:   s = strdup("V_STRING");   break;
        case V_WS:       s = strdup("V_WS");       break;
        case V_COUPLET:  s = strdup("V_COUPLET");  break;
        case V_LABEL:    s = strdup("V_LABEL");    break;
        case V_MANIFOLD: s = strdup("V_MANIFOLD"); break;
        default:
            fprintf(stderr, "illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}

char* w_class_str(Class cls){
    char* s = NULL;
    switch(cls){
        case C_COMPOSON:   s = strdup("C_COMPOSON")   ; break;
        case C_NEST:       s = strdup("C_NEST")       ; break;
        case C_DEREF:      s = strdup("C_DEREF")      ; break;
        case K_LIST:       s = strdup("K_LIST")       ; break;
        case K_PATH:       s = strdup("K_PATH")       ; break;
        case P_WS:         s = strdup("P_WS")         ; break;
        case P_COUPLET:    s = strdup("P_COUPLET")    ; break;
        case P_MANIFOLD:   s = strdup("P_MANIFOLD")   ; break;
        case C_POSITIONAL: s = strdup("C_POSITIONAL") ; break;
        case C_GRPREF:     s = strdup("C_GRPREF")     ; break;
        case P_STRING:     s = strdup("P_STRING")     ; break;
        case K_NAME:       s = strdup("K_NAME")       ; break;
        case T_PATH:       s = strdup("T_PATH")       ; break;
        case T_EFFECT:     s = strdup("T_EFFECT")     ; break;
        case T_CHECK:      s = strdup("T_CHECK")      ; break;
        case T_OPEN:       s = strdup("T_OPEN")       ; break;
        case T_PACK:       s = strdup("T_PACK")       ; break;
        case T_PASS:       s = strdup("T_PASS")       ; break;
        case T_FAIL:       s = strdup("T_FAIL")       ; break;
        case T_DOC:        s = strdup("T_DOC")        ; break;
        case T_CACHE:      s = strdup("T_CACHE")      ; break;
        case C_MANIFOLD:   s = strdup("C_MANIFOLD")   ; break;
        case X_NONE:       s = strdup("X_NONE")       ; break;
        case K_LABEL:      s = strdup("K_LABEL")      ; break;
        default:
            fprintf(stderr, "illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}

bool w_is_recursive(const W* w){
   return get_value_type(w->cls) == V_WS; 
}

#define SEGFAULT fflush(stderr);W* x=0;x->next=0;
void w_assert_class(const W* w, Class cls){
    if(!w){
        fprintf(
            stderr,
            "Class assertion failed! Got NULL, expected %s.\n",
            w_class_str(cls)
        );
SEGFAULT 
    } else if(w->cls != cls){
        fprintf(
            stderr,
            "Class assertion failed! Got %s, expected %s.\n",
            w_class_str(w->cls), w_class_str(cls)
        );
SEGFAULT 
    }
}

void w_assert_type(const W* w, VType type){
    if(!w){
        fprintf(
            stderr,
            "Type assertion failed! Got NULL, expected %s.\n",
            w_type_str(type)
        );
SEGFAULT 
    } else {
        VType t = get_value_type(w->cls);
        if(t != type){
            fprintf(
                stderr,
                "Type assertion failed! Got %s, expected %s.\n",
                w_type_str(t), w_type_str(type)
            );
            if(get_value_type(w->cls) == V_COUPLET){
                fprintf(stderr, "%s\n", w->value.couplet->lhs->value.label->name);
            }
SEGFAULT 
        }
    }
}
#undef SEGFAULT

bool _is_valid_lhs(const W* w){
    bool is_valid;
    switch(w->cls){
        case K_LIST:
        case K_PATH:
        case K_LABEL:
        case K_NAME:
            is_valid = true;
            break;
        default:
            is_valid = false;
            break;
    }
    return is_valid;
}

char* g_string(const W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_STRING);
    return w->value.string;
}
struct Ws* g_ws(const W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_WS);
    return w->value.ws;
}
struct Couplet* g_couplet(const W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_COUPLET);
    return w->value.couplet;
}
struct Label* g_label(const W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_LABEL);
    return w->value.label;
}
struct Manifold* g_manifold(const W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_MANIFOLD);
    return w->value.manifold;
}
W* g_lhs(const W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_COUPLET);
    return w->value.couplet->lhs;
}
W* g_rhs(const W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_COUPLET);
    return w->value.couplet->rhs;
}

void s_none(W* w){
    if(!w) { fprintf(stderr, "Cannot set null in s_none"); return;}
    w_assert_type(w, V_NONE);
    w->value.string = NULL;
}
void s_string(W* w, char* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_string"); return;}
    w_assert_type(w, V_STRING);
    w->value.string = v;
}
void s_ws(W* w, struct Ws* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_ws"); return;}
    w_assert_type(w, V_WS);
    w->value.ws = v;
}
void s_couplet(W* w, Couplet* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_couplet"); return;}
    w_assert_type(w, V_COUPLET);
    w->value.couplet = v;
}
void s_label(W* w, Label* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_label"); return;}
    w_assert_type(w, V_LABEL);
    w->value.label = v;
}
void s_manifold(W* w, Manifold* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_manifold"); return;}
    w_assert_type(w, V_MANIFOLD);
    w->value.manifold = v;
}
void s_lhs(W* w, W* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_lhs"); return;}
    w_assert_type(w, V_COUPLET);
    if(!_is_valid_lhs(v)){
        fprintf(
            stderr,
            "WARNING: illegal value on couplet lhs. Expected K_*, got %s.",
            w_type_str(v->cls) 
        );
    }
    w->value.couplet->lhs = v;
}
void s_rhs(W* w, W* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_rhs"); return;}
    w_assert_type(w, V_COUPLET);
    w->value.couplet->rhs = v;
}

void force_set_none(W* w){
    if(!w) { fprintf(stderr, "Cannot set null in s_none"); return;}
    w->cls = X_NONE;
    s_none(w);
}
void force_set_string(W* w, Class c, char* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_string"); return; }
    if(get_value_type(c) == V_STRING){
        w->cls = c;
        s_string(w, v);
    } else {
        fprintf(stderr, "Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_ws(W* w, Class c, struct Ws* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_ws"); return;}
    if(get_value_type(c) == V_WS){
        w->cls = c;
        s_ws(w, v);
    } else {
        fprintf(stderr, "Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_couplet(W* w, Class c, Couplet* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_couplet"); return;}
    if(get_value_type(c) == V_COUPLET){
        w->cls = c;
        s_couplet(w, v);
    } else {
        fprintf(stderr, "Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_label(W* w, Class c, Label* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_label"); return;}
    if(get_value_type(c) == V_LABEL){
        w->cls = c;
        s_label(w, v);
    } else {
        fprintf(stderr, "Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_manifold(W* w, Class c, Manifold* v){
    if(!w) { fprintf(stderr, "Cannot set null in s_manifold"); return;}
    if(get_value_type(c) == V_MANIFOLD){
        w->cls = c;
        s_manifold(w, v);
    } else {
        fprintf(stderr, "Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
