#ifndef __LHS_H__
#define __LHS_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "ws.h"
#include "types.h"

W* label_from_str(char* s); // W<K_LABEL>

W* list_from_str(char* s);  // W<K_LIST>

W* path_from_str(char* s);  // W<K_PATH>


// ==== ASSIMILATE ME =================================================
// vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

// bool path_is_base(W* path);
//
// Id* id_clone(Id* id);
//
// Id* id_from_str(char* s);
//
// bool id_cmp(Id* a, Id* b);

// ====================================================================

#endif
