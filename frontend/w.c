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

W* w_new(Class cls, void* value){
    static int uid = 0;
    W* w = (W*)calloc(1, sizeof(W));
    w->cls = cls;     
    w->uid = uid++;
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
        case C_COMPOSON:   s = strdup("C_COMPOSON");   break;
        case C_NEST:       s = strdup("C_NEST");       break;
        case C_DEREF:      s = strdup("C_DEREF");      break;
        case K_LIST:       s = strdup("K_LIST");       break;
        case K_PATH:       s = strdup("K_PATH");       break;
        case P_WS:         s = strdup("P_WS");         break;
        case P_COUPLET:    s = strdup("P_COUPLET");    break;
        case P_MANIFOLD:   s = strdup("P_MANIFOLD");   break;
        case C_POSITIONAL: s = strdup("C_POSITIONAL"); break;
        case C_GRPREF:     s = strdup("C_GRPREF");     break;
        case P_STRING:     s = strdup("P_STRING");     break;
        case K_NAME:       s = strdup("K_NAME");       break;
        case T_PATH:       s = strdup("T_PATH");       break;
        case T_EFFECT:     s = strdup("T_EFFECT");     break;
        case C_MANIFOLD:   s = strdup("C_MANIFOLD");   break;
        case X_NONE:       s = strdup("X_NONE");       break;
        case K_LABEL:      s = strdup("K_LABEL");      break;
        default:
            fprintf(stderr, "illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}

bool w_is_recursive(const W* w){
   return get_value_type(w->cls) == V_WS; 
}

char* w_str(const W* w){
    if(!w) return NULL;
    char* s = (char*)malloc(1024 * sizeof(char));
    char* c = w_class_str(w->cls);
    switch(get_value_type(w->cls)){
        case V_NONE:
            sprintf(s, "%s", c);
            break;
        case V_STRING:
            sprintf(s, "%s(%s)", c, w->value.string);
            break;
        case V_WS:
            sprintf(s, "%s", c);
            break;
        case V_COUPLET:
            sprintf(
                s, "%s :: %s | %s", c,
                w_str(w->value.couplet->lhs),
                w_str(w->value.couplet->rhs)
            );
            break;
        case V_LABEL:
            sprintf(
                s, "%s(%s:%s)", c,
                w->value.label->name,
                w->value.label->label
            );
            break;
        case V_MANIFOLD:
            sprintf(s, "%s", c);
            break;
        default:
            fprintf(stderr, "illegal case (%s:%d)\n", __func__, __LINE__);
    }
    return s;
}

/* void w_free(W* o){ }                                                 */
/* W* w_set_value(W* o, void* v){ }                                     */
/* W* w_add_over(W* a, W* b){ }                                         */
/* W* w_add_down(W* a, W* b){ }                                         */
/* W* w_next(const W* o){ }                                             */
/* W* w_down(const W* o){ }                                             */
/* W* w_head(const W* o){ }                                             */
/* W* w_tail(const W* o){ }                                             */
/* W* w_map( const W* o, W*(*map)(W*) ){ }                              */
/* W* w_dmap( const W* o, W*(*map)(W*), int depth ){ }                  */
/* W* w_rmap( const W* o, W*(*map)(W*) ){ }                             */
/* W* w_dig( const W* o, W*(*digger)(W*), W*(*map)(W*) ){ }             */
/* W* w_zmap( const W* a, const W* b, W*(*map)(W*, W*) ){ }             */
/* W* w_zdmap( const W* a, const W* b, W*(*map)(W*, W*), int depth ){ } */
/* W* w_zrmap( const W* a, const W* b, W*(*map)(W*, W*) ){ }            */
/* W* w_filter( const W* o, bool(*filter)(W*) ){ }                      */
/* W* w_flatten(const W* o){ }                                          */
/* bool w_is_alone(const W* o){ }                                       */
/* bool w_is_iterative(const W* o){ }                                   */
/* bool w_is_recursive(const W* o){ }                                   */
/* bool w_is_collective(const W* o){ }                                  */
/* bool w_is_homogenous(const W* o){ }                                  */
/*                                                                      */
/*                                                                      */
/* typedef enum {                                                       */
/*     K_ANON   = 0,                                                    */
/*     K_STRING = 1,                                                    */
/*     K_LABEL  = 2,                                                    */
/*     K_PATH   = 4,                                                    */
/*     K_LIST   = 8                                                     */
/* } KType;                                                             */
/*                                                                      */
/* KType _get_key_type(Class cls){                                      */
/*     KType ktype;                                                     */
/*     switch(cls){                                                     */
/*         case T_PATH:                                                 */
/*             ktype = K_STRING;                                        */
/*             break;                                                   */
/*         case C_COMPOSON:                                             */
/*         case C_NEST:                                                 */
/*         case C_DEREF:                                                */
/*         case C_POSITIONAL:                                           */
/*         case C_GRPREF:                                               */
/*         case T_UNDEFINED:                                            */
/*             ktype = K_ANON;                                          */
/*             break;                                                   */
/*         case C_MANIFOLD:                                             */
/*             ktype = K_LABEL;                                         */
/*             break;                                                   */
/*         case T_EFFECT:                                               */
/*             ktype = K_LIST;                                          */
/*             break;                                                   */
/*     }                                                                */
/*     return ktype;                                                    */
/* }                                                                    */
/*                                                                      */
/* int _get_kmask(VType type){                                          */
/*     int kmask;                                                       */
/*     switch(type){                                                    */
/*         case T_PATH:                                                 */
/*             kmask = K_STRING;                                        */
/*             break;                                                   */
/*         case C_COMPOSON:                                             */
/*         case C_NEST:                                                 */
/*         case C_DEREF:                                                */
/*         case C_POSITIONAL:                                           */
/*         case C_GRPREF:                                               */
/*         case T_UNDEFINED:                                            */
/*             kmask = K_ANON;                                          */
/*             break;                                                   */
/*         case C_MANIFOLD:                                             */
/*             kmask = K_STRING | K_LABEL;                              */
/*             break;                                                   */
/*         case T_EFFECT:                                               */
/*             kmask = K_STRING | K_LABEL | K_LIST;                     */
/*             break;                                                   */
/*     }                                                                */
/*     return kmask;                                                    */
/* }                                                                    */
/*                                                                      */
/* char* _get_class(const Entry* e){                                    */
/*     char* s;                                                         */
/*     switch(e->cls){                                                  */
/*         case C_PATH:                                                 */
/*             s = strdup("C_PATH");                                    */
/*             break;                                                   */
/*         case C_COMPOSON:                                             */
/*             s = strdup("C_COMPOSON");                                */
/*             break;                                                   */
/*         case C_NEST:                                                 */
/*             s = strdup("C_NEST");                                    */
/*             break;                                                   */
/*         case C_DEREF:                                                */
/*             s = strdup("C_DEREF");                                   */
/*             break;                                                   */
/*         case C_MANIFOLD:                                             */
/*             s = strdup("C_MANIFOLD");                                */
/*             break;                                                   */
/*         case C_EFFECT:                                               */
/*             s = strdup("C_EFFECT");                                  */
/*             break;                                                   */
/*         case C_UNDEFINED:                                            */
/*             s = strdup("C_UNDEFINED");                               */
/*             break;                                                   */
/*         case C_GRPREF:                                               */
/*             s = strdup("C_GRPREF");                                  */
/*             break;                                                   */
/*         case C_POSITIONAL:                                           */
/*             s = strdup("C_POSITIONAL");                              */
/*             break;                                                   */
/*     }                                                                */
/*     return s;                                                        */
/* }                                                                    */
