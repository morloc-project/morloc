#ifndef __BUFSTACK_H__
#define __BUFSTACK_H__

#include "lex.yy.h"

#include <unistd.h>
#include <sys/types.h>
#include <pwd.h>

/* My buffer stack handling code draws heavily from: */
/* John Levine (2009) 'Flex and Bison'               */
typedef struct bufstack{
    struct bufstack *prev;    
    YY_BUFFER_STATE bs;
    int lineno;
    char *filename;
    FILE *f;
} bufstack;

/* descend into an included file */ 
int newfile(char *fn);

/* leave a file */
int popfile(void);

#endif
