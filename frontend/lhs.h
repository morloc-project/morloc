#ifndef __LHS_H__
#define __LHS_H__

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "ws.h"
#include "label.h"

W* label_from_str(char* s, char* lang); // W<K_LABEL>

W* list_from_str(char* s, char* lang);  // W<K_LIST>

W* path_from_str(char* s, char* lang);  // W<K_PATH>

#endif
