#include "types.h"

Manifold* manifold_new(){
    static int uid = 0;
    Manifold* m = (Manifold*)calloc(1, sizeof(Manifold));
    m->uid = uid++;
    return m;
}

Label* label_new(){
    return (Label*)calloc(1, sizeof(Label));
}

Label* label_new_set(char* name, char* label){
    Label* l = (Label*)malloc(sizeof(Label));
    l->name = name;
    l->label = label;
    return l;
}

Couplet* couplet_new(struct W* lhs, struct W* rhs){
    Couplet* c = (Couplet*)malloc(sizeof(Couplet));
    c->lhs = lhs;
    c->rhs = rhs;
    return c;
}

bool label_cmp(Label* a, Label* b){
    return
        a && b &&
        a->name && b->name &&
        strcmp(a->name, b->name) == 0 &&
        (
            (a->label == NULL && b->label == NULL) ||
            strcmp(a->label, b->label) == 0
        )
    ;
}

// ==== ASSIMILATE ME =================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

/* Entry* entry_new(VType type, void* key, void* value){                      */
/*                                                                            */
/*     static int uid = 0;                                                    */
/*                                                                            */
/*     Entry* e = (Entry*)malloc(sizeof(Entry));                              */
/*     e->type = type;                                                        */
/*     e->uid = uid++;                                                        */
/*     e->next = NULL;                                                        */
/*                                                                            */
/*     switch(_get_key_type(type)){                                           */
/*         case K_ANON:                                                       */
/*             e->key.none = NULL;                                            */
/*             break;                                                         */
/*         case K_STRING:                                                     */
/*             e->key.string = value;                                         */
/*             break;                                                         */
/*         case K_LABEL:                                                      */
/*             e->key.label = value;                                          */
/*             break;                                                         */
/*         case K_PATH:                                                       */
/*             e->key.path = value;                                           */
/*         case K_LIST:                                                       */
/*             e->key.list = value;                                           */
/*             break;                                                         */
/*     }                                                                      */
/*                                                                            */
/*     switch(_get_value_type(type)){                                         */
/*         case V_TABLE:                                                      */
/*             e->value.table = value;                                        */
/*             break;                                                         */
/*         case V_MANIFOLD:                                                   */
/*             e->value.manifold = value;                                     */
/*             break;                                                         */
/*         case V_STRING:                                                     */
/*             e->value.string = value;                                       */
/*             break;                                                         */
/*         case V_EFFECT:                                                     */
/*             e->value.effect = value;                                       */
/*             break;                                                         */
/*         case V_NONE:                                                       */
/*             fprintf(stderr, "Undefined type\n");                           */
/*             exit(EXIT_FAILURE);                                            */
/*             break;                                                         */
/*     }                                                                      */
/*                                                                            */
/*     return e;                                                              */
/* }                                                                          */
/*                                                                            */
/* void entry_print(const Entry* e){                                          */
/*                                                                            */
/*     if(!e){                                                                */
/*         printf("null entry\n");                                            */
/*         return;                                                            */
/*     }                                                                      */
/*                                                                            */
/*     switch(_get_key_type(e->cls)){                                         */
/*         case K_ANON:                                                       */
/*             printf("%s anonymous \n", _get_class(e));                      */
/*             break;                                                         */
/*         case K_STRING:                                                     */
/*             printf("%s %s\n", _get_class(e), e->key.string);               */
/*             break;                                                         */
/*         case K_LABEL:                                                      */
/*             printf("%s:%s %s\n",                                           */
/*                 _get_class(e),                                             */
/*                 e->key.label->name,                                        */
/*                 e->key.label->label                                        */
/*             );                                                             */
/*             break;                                                         */
/*         case K_PATH:                                                       */
/*             printf("%s %s\n", _get_class(e), path_show(e->key.selection)); */
/*             break;                                                         */
/*         case K_LIST:                                                       */
/*             printf("%s %s\n", _get_class(e), list_show(e->key.selection)); */
/*             break;                                                         */
/*     }                                                                      */
/*                                                                            */
/* }                                                                          */
/*                                                                            */
/* Entry* entry_copy(const Entry* e){                                         */
/*    Entry* new_entry = (Entry*)malloc(sizeof(Entry));                       */
/*    memcpy(new_entry, e, sizeof(Entry));                                    */
/*    return new_entry;                                                       */
/* }                                                                          */
/*                                                                            */
/* Entry* entry_isolate(const Entry* e){                                      */
/*     Entry* new_entry = entry_copy(e);                                      */
/*     new_entry->next = NULL;                                                */
/*     return new_entry;                                                      */
/* }                                                                          */
/*                                                                            */
/* Entry* entry_from_lhs(Class cls, const char* s){                           */
/*     int kmask = _get_kmask(cls);                                           */
/*     if(kmask & K_ANON){                                                    */
/*                                                                            */
/*         return e;                                                          */
/*     }                                                                      */
/*                                                                            */
/*     if(kmask & K_LIST){ }                                                  */
/*     if(kmask & K_PATH){ }                                                  */
/*     if(kmask & K_LABEL){ }                                                 */
/*                                                                            */
/*     return e;                                                              */
/* }                                                                          */
