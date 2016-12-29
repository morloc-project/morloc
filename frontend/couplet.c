#include "entry.h"

typedef enum {
    V_NONE = 0,
    V_STRING,
    V_TABLE,
    V_MANIFOLD,
    V_EFFECT
} VType;

typedef enum {
    K_ANON   = 0,
    K_STRING = 1,
    K_LABEL  = 2,
    K_PATH   = 4,
    K_LIST   = 8
} KType;

KType _get_key_type(Class cls){
    KType ktype;
    switch(cls){
        case T_PATH:
            ktype = K_STRING;
            break;
        case C_COMPOSON:
        case C_NEST:
        case C_DEREF:
        case C_POSITIONAL:
        case C_GRPREF:
        case T_UNDEFINED:
            ktype = K_ANON;
            break;
        case C_MANIFOLD:
            ktype = K_LABEL;
            break;
        case T_EFFECT:
            ktype = K_LIST;
            break;
    }
    return ktype;
}

int _get_kmask(VType type){
    int kmask;
    switch(type){
        case T_PATH:
            kmask = K_STRING;
            break;
        case C_COMPOSON:
        case C_NEST:
        case C_DEREF:
        case C_POSITIONAL:
        case C_GRPREF:
        case T_UNDEFINED:
            kmask = K_ANON;
            break;
        case C_MANIFOLD:
            kmask = K_STRING | K_LABEL;
            break;
        case T_EFFECT:
            kmask = K_STRING | K_LABEL | K_LIST;
            break;
    }
    return kmask;
}

VType _get_value_type(Class cls){
    VType vtype;
    switch(cls){
        case C_PATH:
        case C_COMPOSON:
        case C_NEST:
        case C_DEREF:
            vtype = V_TABLE;
            break;
        case C_MANIFOLD:
            vtype = V_MANIFOLD;
            break;
        case C_POSITIONAL:
        case C_GRPREF:
            vtype = V_STRING;
            break;
        case C_EFFECT:
            vtype = V_EFFECT;
            break;
        case C_NONE:
            vtype = V_NONE;
    }
    return vtype;
}

char* _get_class(const Entry* e){
    char* s;
    switch(e->cls){
        case C_PATH:
            s = strdup("C_PATH");
            break;
        case C_COMPOSON:
            s = strdup("C_COMPOSON");
            break;
        case C_NEST:
            s = strdup("C_NEST");
            break;
        case C_DEREF:
            s = strdup("C_DEREF");
            break;
        case C_MANIFOLD:
            s = strdup("C_MANIFOLD");
            break;
        case C_EFFECT:
            s = strdup("C_EFFECT");
            break;
        case C_UNDEFINED:
            s = strdup("C_UNDEFINED");
            break;
        case C_GRPREF:
            s = strdup("C_GRPREF");
            break;
        case C_POSITIONAL:
            s = strdup("C_POSITIONAL");
            break;
    }
    return s;
}

Entry* entry_new(VType type, void* key, void* value){

    static int uid = 0;

    Entry* e = (Entry*)malloc(sizeof(Entry));
    e->type = type;
    e->uid = uid++;
    e->next = NULL;

    switch(_get_key_type(type)){
        case K_ANON:
            e->key.none = NULL;
            break;
        case K_STRING:
            e->key.string = value;
            break;
        case K_LABEL:
            e->key.label = value;
            break;
        case K_PATH:
            e->key.path = value;
        case K_LIST:
            e->key.list = value;
            break;
    }

    switch(_get_value_type(type)){
        case V_TABLE:
            e->value.table = value;
            break;
        case V_MANIFOLD:
            e->value.manifold = value;
            break;
        case V_STRING:
            e->value.string = value;
            break;
        case V_EFFECT:
            e->value.effect = value;
            break;
        case V_NONE:
            fprintf(stderr, "Undefined type\n");
            exit(EXIT_FAILURE);
            break;
    }

    return e;
}

Entry* entry_copy(const Entry* e){
   Entry* new_entry = (Entry*)malloc(sizeof(Entry));
   memcpy(new_entry, e, sizeof(Entry)); 
   return new_entry;
}

Entry* entry_isolate(const Entry* e){
    Entry* new_entry = entry_copy(e);
    new_entry->next = NULL;
    return new_entry;
}

void entry_print(const Entry* e){

    if(!e){
        printf("null entry\n");
        return;
    }

    switch(_get_key_type(e->cls)){
        case K_ANON:
            printf("%s anonymous \n", _get_class(e));
            break;
        case K_STRING:
            printf("%s %s\n", _get_class(e), e->key.string);
            break;
        case K_LABEL:
            printf("%s:%s %s\n",
                _get_class(e),
                e->key.label->name,
                e->key.label->label
            );
            break;
        case K_PATH:
            printf("%s %s\n", _get_class(e), path_show(e->key.selection));
            break;
        case K_LIST:
            printf("%s %s\n", _get_class(e), list_show(e->key.selection));
            break;
    }

}

Entry* entry_from_lhs(Class cls, const char* s){
    int kmask = _get_kmask(cls); 
    if(kmask & K_ANON){

        return e;
    }

    if(kmask & K_LIST){ }
    if(kmask & K_PATH){ }
    if(kmask & K_LABEL){ }

    return e;
}

Entry* entry_add_rhs(Entry* entry, void* rhs){

}




/* Id* id_new(){                           */
/*     static int uid = 0;                 */
/*     Id* i = (Id*)calloc(1, sizeof(Id)); */
/*     i->uid = uid++;                     */
/*     return i;                           */
/* }                                       */

/* Id* id_clone(Id* id){                                    */
/*                                                          */
/*     if(!id) return NULL;                                 */
/*                                                          */
/*     Id* newid = id_new();                                */
/*     newid->name = id->name ? strdup(id->name) : NULL;    */
/*     newid->label = id->label ? strdup(id->label) : NULL; */
/*                                                          */
/*     return newid;                                        */
/* }                                                        */

/* Id* id_from_str(char* s){                           */
/*     Id* id = id_new();                              */
/*     int i = 0;                                      */
/*     for(;;i++){                                     */
/*         if(s[i] == '\0'){                           */
/*             break;                                  */
/*         }                                           */
/*         else if(s[i] == ':'){                       */
/*             id->label = strdup(s + i + 1);          */
/*             break;                                  */
/*         }                                           */
/*     }                                               */
/*     id->name = (char*)malloc((i+1) * sizeof(char)); */
/*     id->name = memcpy(id->name, s, i*sizeof(char)); */
/*     id->name[i] = '\0';                             */
/*                                                     */
/*     id->name = trim_ws(id->name);                   */
/*     id->label = trim_ws(id->label);                 */
/*                                                     */
/*     return id;                                      */
/* }                                                   */

/* bool id_cmp(Id* a, Id* b){                                                         */
/*     if(!a || !b){                                                                  */
/*         fprintf(stderr, "WARNING: cannot compare null ids\n"); fflush(stderr);     */
/*     }                                                                              */
/*     if(!a->name || !b->name){                                                      */
/*         fprintf(stderr, "WARNING: cannot compare nameless ids\n"); fflush(stderr); */
/*     }                                                                              */
/*     bool result =                                                                  */
/*         strcmp(a->name, b->name) == 0 &&                                           */
/*         (                                                                          */
/*             (a->label == NULL && b->label == NULL) ||                              */
/*             (                                                                      */
/*                 (a->label != NULL && b->label != NULL) &&                          */
/*                 strcmp(a->label, b->label) == 0                                    */
/*             )                                                                      */
/*         );                                                                         */
/*     return result;                                                                 */
/* }                                                                                  */
