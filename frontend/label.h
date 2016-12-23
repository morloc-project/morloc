#ifndef __LABEL_H__
#define __LABEL_H__

#include <stdlib.h>

typedef struct Label{
    struct Label* next;
    char* name; 
} Label;
Label* label_new(char* name);

#endif
