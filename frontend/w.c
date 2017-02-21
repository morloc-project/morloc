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
        case FT_FUNCTION:
        case FT_TUPLE:
        case FT_ARRAY:
            vtype = V_WS;
            break;
        case C_GRPREF:
        case C_ARGREF:
        case P_STRING:
        case K_NAME:
        case FT_ATOMIC:
            vtype = V_STRING;
            break;
        case T_PATH:
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
        case C_POSITIONAL: // Couplet<type:P_STRING,value:P_STRING>
        case T_CHECK:
        case T_FAIL:
        case T_ALIAS:
        case T_LANG:
        case T_DOC:
        case T_CACHE:
        case C_MANIFOLD:
        case T_SECTION:
        case C_REFER:
        case P_COUPLET:
        case T_SOURCE:
        case T_ARGUMENT:
        case P_ARGUMENT:
        case T_TYPE:
        case T_ONTOLOGY:
        case T_EXPORT:
            vtype = V_COUPLET;
            break;
        case K_LABEL:
            vtype = V_LABEL;
            break;
        case P_MANIFOLD:
            vtype = V_MANIFOLD;
            break;
        case P_SECTION:
            vtype = V_SECTION;
            break;
        case X_NONE:
            vtype = V_NONE;
            break;
        default:
            warn("illegal case %d (%s:%d)\n", cls, __func__, __LINE__);
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
        case V_SECTION:
            w->value.section = value;
            break;
        default:
            warn("illegal case (%s:%d)\n", __func__, __LINE__);
    }

    return w;
}

W* w_isolate(W* w){
    if(!w) return NULL;
    W* new_w = w_copy(w);
    new_w->next = NULL;
    return new_w;
}

W* w_copy(W* w){
    if(!w) return NULL;
    W* new_w = (W*)malloc(sizeof(W));
    memcpy(new_w, w, sizeof(W));
    return new_w;
}

W* w_clone(W* w){
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
        case V_SECTION:  s = strdup("V_SECTION");  break;
        default:
            warn("illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}

char* w_class_str(Class cls){
    char* s = NULL;
    switch(cls){
        case C_ARGREF:     s = strdup("C_ARGREF")     ; break;
        case C_COMPOSON:   s = strdup("C_COMPOSON")   ; break;
        case C_DEREF:      s = strdup("C_DEREF")      ; break;
        case C_GRPREF:     s = strdup("C_GRPREF")     ; break;
        case C_MANIFOLD:   s = strdup("C_MANIFOLD")   ; break;
        case C_NEST:       s = strdup("C_NEST")       ; break;
        case C_POSITIONAL: s = strdup("C_POSITIONAL") ; break;
        case C_REFER:      s = strdup("C_REFER")      ; break;
        case K_LABEL:      s = strdup("K_LABEL")      ; break;
        case K_LIST:       s = strdup("K_LIST")       ; break;
        case K_NAME:       s = strdup("K_NAME")       ; break;
        case K_PATH:       s = strdup("K_PATH")       ; break;
        case P_ARGUMENT:   s = strdup("P_ARGUMENT")   ; break;
        case P_COUPLET:    s = strdup("P_COUPLET")    ; break;
        case P_MANIFOLD:   s = strdup("P_MANIFOLD")   ; break;
        case P_SECTION:    s = strdup("P_SECTION")    ; break;
        case P_STRING:     s = strdup("P_STRING")     ; break;
        case FT_FUNCTION:  s = strdup("FT_FUNCTION")  ; break;
        case FT_TUPLE:     s = strdup("FT_TUPLE")     ; break;
        case FT_ARRAY:     s = strdup("FT_ARRAY")     ; break;
        case FT_ATOMIC:    s = strdup("FT_ATOMIC")    ; break;
        case P_WS:         s = strdup("P_WS")         ; break;
        case T_ARGUMENT:   s = strdup("T_ARGUMENT")   ; break;
        case T_ALIAS:      s = strdup("T_ALIAS")      ; break;
        case T_CACHE:      s = strdup("T_CACHE")      ; break;
        case T_CHECK:      s = strdup("T_CHECK")      ; break;
        case T_DOC:        s = strdup("T_DOC")        ; break;
        case T_EXPORT:     s = strdup("T_EXPORT")     ; break;
        case T_FAIL:       s = strdup("T_FAIL")       ; break;
        case T_H0:         s = strdup("T_H0")         ; break;
        case T_H1:         s = strdup("T_H1")         ; break;
        case T_H2:         s = strdup("T_H2")         ; break;
        case T_H3:         s = strdup("T_H3")         ; break;
        case T_H4:         s = strdup("T_H4")         ; break;
        case T_H5:         s = strdup("T_H5")         ; break;
        case T_H6:         s = strdup("T_H6")         ; break;
        case T_H7:         s = strdup("T_H7")         ; break;
        case T_H8:         s = strdup("T_H8")         ; break;
        case T_H9:         s = strdup("T_H9")         ; break;
        case T_LANG:       s = strdup("T_LANG")       ; break;
        case T_ONTOLOGY:   s = strdup("T_ONTOLOGY")   ; break;
        case T_PATH:       s = strdup("T_PATH")       ; break;
        case T_SECTION:    s = strdup("T_SECTION")    ; break;
        case T_SOURCE:     s = strdup("T_SOURCE")     ; break;
        case T_TYPE:       s = strdup("T_TYPE")       ; break;
        case X_NONE:       s = strdup("X_NONE")       ; break;
        default:
            warn("illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}

#define SEGFAULT fflush(stderr);W* x=0;x->next=0;
void w_assert_class(W* w, Class cls){
    if(!w){
        warn(
            "Class assertion failed! Got NULL, expected %s.\n",
            w_class_str(cls)
        );
        SEGFAULT 
    } else if(w->cls != cls){
        warn(
            "Class assertion failed! Got %s, expected %s.\n",
            w_class_str(w->cls),
            w_class_str(cls)
        );
        SEGFAULT 
    }
}

void w_assert_type(W* w, VType type){
    if(!w){
        warn(
            "Type assertion failed! Got NULL, expected %s.\n",
            w_type_str(type)
        );
        SEGFAULT 
    } else {
        VType t = get_value_type(w->cls);
        if(t != type){
            warn(
                "W type error! Got %s, expected %s.\n",
                w_type_str(t),
                w_type_str(type)
            );
            if(get_value_type(w->cls) == V_COUPLET){
                warn("%s\n", w->value.couplet->lhs->value.label->name);
            }
            SEGFAULT 
        }
    }
}
#undef SEGFAULT

bool _is_valid_lhs(W* w){
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

char* g_string(W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_STRING);
    return w->value.string;
}
struct Ws* g_ws(W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_WS);
    return w->value.ws;
}
struct Couplet* g_couplet(W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_COUPLET);
    return w->value.couplet;
}
struct Label* g_label(W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_LABEL);
    return w->value.label;
}
struct Manifold* g_manifold(W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_MANIFOLD);
    return w->value.manifold;
}
struct Section* g_section(W* w) {
    if(!w) return NULL;
    w_assert_type(w, V_SECTION);
    return w->value.section;
}
W* g_lhs(W* w) {
    if(!w || !w->value.couplet) return NULL;
    w_assert_type(w, V_COUPLET);
    return w->value.couplet->lhs;
}
W* g_rhs(W* w) {
    if(!w || !w->value.couplet) return NULL;
    w_assert_type(w, V_COUPLET);
    return w->value.couplet->rhs;
}

void s_none(W* w){
    if(!w) { warn("Cannot set null in s_none\n"); return;}
    w_assert_type(w, V_NONE);
    w->value.string = NULL;
}
void s_string(W* w, char* v){
    if(!w) { warn("Cannot set null in s_string\n"); return;}
    w_assert_type(w, V_STRING);
    w->value.string = v;
}
void s_ws(W* w, struct Ws* v){
    if(!w) { warn("Cannot set null in s_ws\n"); return;}
    w_assert_type(w, V_WS);
    w->value.ws = v;
}
void s_couplet(W* w, Couplet* v){
    if(!w) { warn("Cannot set null in s_couplet\n"); return;}
    w_assert_type(w, V_COUPLET);
    w->value.couplet = v;
}
void s_label(W* w, Label* v){
    if(!w) { warn("Cannot set null in s_label\n"); return;}
    w_assert_type(w, V_LABEL);
    w->value.label = v;
}
void s_manifold(W* w, Manifold* v){
    if(!w) { warn("Cannot set null in s_manifold\n"); return;}
    w_assert_type(w, V_MANIFOLD);
    w->value.manifold = v;
}
void s_section(W* w, Section* v){
    if(!w) { warn("Cannot set null in s_section\n"); return;}
    w_assert_type(w, V_SECTION);
    w->value.section = v;
}
void s_lhs(W* w, W* v){
    if(!w) { warn("Cannot set null in s_lhs\n"); return;}
    w_assert_type(w, V_COUPLET);
    w->value.couplet->lhs = v;
}
void s_rhs(W* w, W* v){
    if(!w) { warn("Cannot set null in s_rhs\n"); return;}
    w_assert_type(w, V_COUPLET);
    w->value.couplet->rhs = v;
}

void force_set_none(W* w){
    if(!w) { warn("Cannot set null in s_none\n"); return;}
    w->cls = X_NONE;
    s_none(w);
}
void force_set_string(W* w, Class c, char* v){
    if(!w) { warn("Cannot set null in s_string\n"); return; }
    if(get_value_type(c) == V_STRING){
        w->cls = c;
        s_string(w, v);
    } else {
        warn("Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_ws(W* w, Class c, struct Ws* v){
    if(!w) { warn("Cannot set null in s_ws\n"); return;}
    if(get_value_type(c) == V_WS){
        w->cls = c;
        s_ws(w, v);
    } else {
        warn("Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_couplet(W* w, Class c, Couplet* v){
    if(!w) { warn("Cannot set null in s_couplet\n"); return;}
    if(get_value_type(c) == V_COUPLET){
        w->cls = c;
        s_couplet(w, v);
    } else {
        warn("Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_label(W* w, Class c, Label* v){
    if(!w) { warn("Cannot set null in s_label\n"); return;}
    if(get_value_type(c) == V_LABEL){
        w->cls = c;
        s_label(w, v);
    } else {
        warn("Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
void force_set_manifold(W* w, Class c, Manifold* v){
    if(!w) { warn("Cannot set null in s_manifold\n"); return;}
    if(get_value_type(c) == V_MANIFOLD){
        w->cls = c;
        s_manifold(w, v);
    } else {
        warn("Invalid type in %s:%d\n", __func__, __LINE__);
    }
}
