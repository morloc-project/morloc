#include "entry.h"

Entry* entry_new(Id* id, TType type, void* value){
    Entry* e = (Entry*)malloc(sizeof(Entry));
    e->type = type;
    e->id = id;
    e->next = NULL;

    switch(type){
        case T_PATH:
        case C_COMPOSON:
        case C_NEST:
        case C_DEREF:
            e->value.table = value;
            break;
        case C_MANIFOLD:
            e->value.manifold = value;
            break;
        case C_POSITIONAL:
        case C_GRPREF:
            e->value.string = value;
            break;
        case T_EFFECT:
            e->value.effect = value;
            break;
        case T_UNDEFINED:
            fprintf(stderr, "UNDEFINED TYPE\n");
            exit(EXIT_FAILURE);
            break;
        default:
            fprintf(stderr, "ILLEGAL TYPE\n");
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

char* type_str(const Entry* e){
    char* s;
    switch(e->type){
        case T_PATH:
            s = strdup("T_PATH");
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
        case T_EFFECT:
            s = strdup("T_EFFECT");
            break;
        case T_UNDEFINED:
            s = strdup("T_UNDEFINED");
            break;
        case C_GRPREF:
            s = strdup("C_GRPREF");
            break;
        case C_POSITIONAL:
            s = strdup("C_POSITIONAL");
            break;
        default:
            s = strdup("ILLEGAL");
            break;
    }
    return s;
}

void entry_print(const Entry* e){
    if(e){
        if(e->id){
            if(e->id->label){
                printf("%s:%s %s\n",
                    e->id->name,
                    e->id->label,
                    type_str(e)
                );
            } else {
                printf("%s %s\n",
                    e->id->name,
                    type_str(e)
                );
            }
        } else {
            printf("noid %s\n", type_str(e));
        }
    } else {
        printf("null_entry\n");
    }
}
