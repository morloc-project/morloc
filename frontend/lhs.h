#ifndef __LHS_H__
#define __LHS_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

typedef struct Label{
    char* name;
    char* label;
} Label;

Label* label_from_str(char* s);

// ==== ASSIMILATE ME =================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

// W* selection_from_str(char* s);
//
// W* path_from_selection(W* selection);
//
// W* label_from_path(W* path);
//
// bool path_is_base(W* path);
//
// Id* id_clone(Id* id);
//
// Id* id_from_str(char* s);
//
// bool id_cmp(Id* a, Id* b);

// ====================================================================

#endif
