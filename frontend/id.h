#ifndef __ID_H__
#define __ID_H__

#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "util.h"

typedef struct Id{
    char* name;
    char* label; 
    int uid;
} Id;

Id* id_new();

Id* id_from_str(char* s);

bool id_cmp(Id* a, Id* b);

#endif
