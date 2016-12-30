#include "w.h"

typedef enum {
    V_NONE = 0,
    V_STRING,
    V_WS,
    V_COUPLET,
    V_LABEL
} VType;

VType _get_value_type(Class cls){
    VType vtype;
    switch(cls){
        case C_COMPOSON:
        case C_NEST:
        case C_DEREF:
        case K_LIST:
        case K_PATH:
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
            vtype = V_COUPLET;
            break;
        case X_NONE:
            vtype = V_NONE;
            break;
        case K_LABEL:
            vtype = V_LABEL;
            break;
        default:
            fprintf(stderr, "Illegal VType: %d\n", cls);
    }
    return vtype;

}

W* w_new(Class cls, void* value){
    static int uid = 0;
    W* w = (W*)malloc(sizeof(W));
    w->cls = cls;     
    w->uid = uid++;
    w->next = NULL;

    switch(_get_value_type(cls)){
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
        default:
            break;
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
