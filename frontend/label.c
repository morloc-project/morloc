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

// assume 'a' is being searched against 'b'
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
                // AND EITHER
                (
                    // the query has no label
                    a->label == NULL ||
                    // OR both labeled identically
                    (
                        b->label != NULL &&
                        strcmp(a->label, b->label) == 0
                    )
                )
            )
        )
    ;
}
