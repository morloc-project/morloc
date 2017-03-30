#include "bufstack.h"

char* yyfilename = NULL;

bufstack *current_bs = NULL;

int newfile(char* fn){
    FILE *f = fopen(fn, "r");

    char* home = getenv("HOME");

    if(f == NULL){
        char* lib = ".morloc/lib";
        int size = strlen(fn) + strlen(home) + strlen(lib) + 3;
        char* libfn = (char*)malloc(size * sizeof(char));
        sprintf(libfn, "%s/%s/%s", home, lib, fn);
        f = fopen(libfn, "r");
        if(f == NULL){
            warn("Could not find '%s'\n", libfn);
            return 1;
        }
        free(libfn);
    }

    struct bufstack *bs = malloc(sizeof(struct bufstack));
    if(bs == NULL){
        warn("malloc error\n");
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
    yyfilename = bs->filename;
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
    yyfilename = current_bs->filename;

    return 1;
}
