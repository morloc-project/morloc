#include "bufstack.h"

/* global variable for storing filename */
char* current_filename = NULL;

/* global variable for storing buffer stack */
bufstack *current_bs = NULL;

int newfile(char* fn){
    FILE *f = fopen(fn, "r");
    if(f == NULL){
        fprintf(stderr, "Could not open file '%s'\n", fn);
        return 0;
    }

    /* printf(" --- %s --- \n", fn); */

    struct bufstack *bs = malloc(sizeof(struct bufstack));
    if(bs == NULL){
        fprintf(stderr, "malloc error\n");
        return 0;
    }

    if(current_bs != NULL) current_bs->lineno = yylineno;

    bs->prev     = current_bs;
    bs->bs       = yy_create_buffer(f, YY_BUF_SIZE);
    bs->lineno   = 1;
    bs->filename = strdup(fn);
    bs->f        = f;

    yy_switch_to_buffer(bs->bs);

    current_bs = bs;
    current_filename = bs->filename;
    yylineno = 1;

    return 1;
}

int popfile(void){
    struct bufstack *bs = current_bs;
    struct bufstack *prevbs;

    if(bs == NULL) return 0;

    fclose(bs->f);
    free(bs->filename);
    yy_delete_buffer(bs->bs);

    prevbs = current_bs->prev;
    free(bs);

    if(prevbs == NULL) return 0;

    yy_switch_to_buffer(prevbs->bs);
    current_bs = prevbs;

    yylineno = current_bs->lineno;
    current_filename = current_bs->filename;

    /* printf(" --- %s --- \n", current_filename); */

    return 1;
}
