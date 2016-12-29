#ifndef __SELECTION_H__
#define __SELECTION_H__

#include <stdlib.h>
#include <stdio.h>

#include "path.h"

typedef struct Selection{
    struct Selection* next;
    Path* path; 
} Selection;

Selection* selection_from_str(char* s);

#endif
