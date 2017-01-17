#include "label.h"

Label* label_new(){
    return (Label*)calloc(1, sizeof(Label));
}

Label* label_new_set(char* name, char* label){
    Label* l = (Label*)malloc(sizeof(Label));
    l->name = name;
    l->label = label;
    return l;
}

Label* label_copy(Label* l){
    char* name  = l->name  ? strdup(l->name)  : NULL;
    char* label = l->label ? strdup(l->label) : NULL;
    return label_new_set(name, label);
}

bool label_cmp(Label* a, Label* b){
    return
        // both must exist
        ( a && b )
        &&
        // both must have names
        ( a->name && b->name )
        // names must match
        &&
        (
            // EITHER one must be wild
            (
                strcmp(a->name, "*") == 0 || strcmp(b->name, "*") == 0
            )
            ||
            // OR
            (
                // names are equal
                strcmp(a->name, b->name) == 0
                &&
                // AND labels are either both missing or equal
                (
                    (a->label == NULL && b->label == NULL) ||
                    strcmp(a->label, b->label) == 0
                )
            )
        )
    ;
}
