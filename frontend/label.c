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
        a && b &&
        a->name && b->name &&
        strcmp(a->name, b->name) == 0 &&
        (
            (a->label == NULL && b->label == NULL) ||
            strcmp(a->label, b->label) == 0
        )
    ;
}
