#ifndef __PARSE_LHS_H__
#define __PARSE_LHS_H__


#include <stdlib.h>
#include <stdio.h>

#include "path.h"

W* selection_from_str(char* s);

W* path_from_selection(W* selection);

W* label_from_path(W* path);

bool path_is_base(W* path);

#endif
