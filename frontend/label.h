#ifndef __LABEL_H__
#define __LABEL_H__

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

typedef struct Label{
    char* name;
    char* label;
    char* lang;
} Label;

Label* label_new();

// creates a new label with copies of the original strings
Label* label_copy(Label*);

Label* label_new_set(char* name, char* label, char* lang);

bool label_cmp(Label* a, Label* b);

#endif
