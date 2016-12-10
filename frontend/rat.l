%{
    #include "rat.tab.h"

/* My buffer stack handling code draws heavily from: */
/* John Levine (2009) 'Flex and Bison'               */
struct bufstack {
    struct bufstack *prev;    
    YY_BUFFER_STATE bs;
    int lineno;
    char *filename;
    FILE *f;
} *current_bs = NULL;

char* current_filename;

int newfile(char *fn);
int popfile(void);

/*
{comment} { printf( "COMMENT\n"); }
{section_import}  { printf( "SECTION_IMPORT"  ); BEGIN S_IMPORT; }
{section_export}  { printf( "SECTION_EXPORT"  ); BEGIN INITIAL;  }
{section_path}    { printf( "SECTION_PATH"    ); BEGIN INITIAL;  }
{section_compose} { printf( "SECTION_COMPOSE" ); BEGIN INITIAL;  }
{section_alias}   { printf( "SECTION_ALIAS"   ); BEGIN INITIAL;  }
{section_arg}     { printf( "SECTION_ARG"     ); BEGIN INITIAL;  }
{section_cache}   { printf( "SECTION_CACHE"   ); BEGIN INITIAL;  }
{section_check}   { printf( "SECTION_CHECK"   ); BEGIN INITIAL;  }
{section_effect}  { printf( "SECTION_EFFECT"  ); BEGIN INITIAL;  }
{section_pack}    { printf( "SECTION_PACK"    ); BEGIN INITIAL;  }
{section_open}    { printf( "SECTION_OPEN"    ); BEGIN INITIAL;  }
{section_fail}    { printf( "SECTION_FAIL"    ); BEGIN INITIAL;  }
{section_pass}    { printf( "SECTION_PASS"    ); BEGIN INITIAL;  }
{section_loop}    { printf( "SECTION_LOOP"    ); BEGIN INITIAL;  }
<S_IMPORT>{str} {
                  yytext++;
                  yytext[strlen(yytext)-1]='\0';
                  if(!newfile(yytext)) yyterminate();
                }
<<EOF>>         { if(!popfile()) yyterminate(); }
{eol}           { printf("\n"); }
{ws}            { }
{with}    { printf( "WITH "    ); }
{using}   { printf( "USING "   ); }
{split}   { printf( "SPLIT "   ); }
{on}      { printf( "ON "      ); }
{as}      { printf( "AS "      ); }
{merge}   { printf( "MERGE "   ); }
{builtin} { printf( "BUILTIN " ); }
{group}   { printf( "GROUP "   ); }
{str}     { printf( "STR "     ); }
{var}     { printf( "VAR "     ); }
{dbl}     { printf( "DBL "     ); }
{int}     { printf( "INT "     ); }
{dep}     { printf( "DEP "     ); }
{eos}     { printf( "EOS "     ); }
{compose} { printf( "COMPOSE " ); }
{equal}   { printf( "EQUAL "   ); }
{couple}  { printf( "COUPLE "  ); }
{label}   { printf( "LABEL "   ); }
{lbrk}    { printf( "LBRK "    ); }
{rbrk}    { printf( "RBRK "    ); }
{lpar}    { printf( "LPAR "    ); }
{rpar}    { printf( "RPAR "    ); }
{sep}     { printf( "SEP "     ); }
*/

%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap pointer yylineno

%s S_IMPORT

comment #.*\n
ws      [ \t\n\r]+
not_ws  [^ \t\n\r]+
int     [0-9]|[1-9][0-9]+
dbl     {int}\.[0-9]+
str     \'[^']*\'|\"[^"]*\"
var     [a-zA-Z_][a-zA-Z_0-9]*
dep     -->
eol     \n

compose "."
eos     ;|---+
equal   =
couple  ::
label   :
cond    "?"

lbrk    \[
rbrk    \]
lpar    \(
rpar    \)
sep     ,

builtin (memcache|datcache|nocache|true|null|id|call)

as    as
with  with
split split
on    on
merge merge
using using

group   __all__

section_export  @export
section_import  @import
section_path    @path
section_compose @compose
section_alias   @alias
section_arg     @arg
section_cache   @cache
section_check   @check
section_effect  @effect
section_pack    @pack
section_open    @open
section_fail    @fail
section_pass    @pass
section_loop    @loop

%%

{comment} { }
{ws} { }

{section_import}  { BEGIN S_IMPORT ; }
{section_export}  { BEGIN INITIAL  ; return SECTION_EXPORT  ; }
{section_path}    { BEGIN INITIAL  ; return SECTION_PATH    ; }
{section_compose} { BEGIN INITIAL  ; return SECTION_COMPOSE ; }
{section_alias}   { BEGIN INITIAL  ; return SECTION_ALIAS   ; }
{section_arg}     { BEGIN INITIAL  ; return SECTION_ARG     ; }
{section_cache}   { BEGIN INITIAL  ; return SECTION_CACHE   ; }
{section_check}   { BEGIN INITIAL  ; return SECTION_CHECK   ; }
{section_effect}  { BEGIN INITIAL  ; return SECTION_EFFECT  ; }
{section_pack}    { BEGIN INITIAL  ; return SECTION_PACK    ; }
{section_open}    { BEGIN INITIAL  ; return SECTION_OPEN    ; }
{section_fail}    { BEGIN INITIAL  ; return SECTION_FAIL    ; }
{section_pass}    { BEGIN INITIAL  ; return SECTION_PASS    ; }
{section_loop}    { BEGIN INITIAL  ; return SECTION_LOOP    ; }

<S_IMPORT>{str} {
                  yytext++;
                  yytext[strlen(yytext)-1]='\0';
                  if(!newfile(yytext)) yyterminate();
                }
<<EOF>>         { if(!popfile()) yyterminate(); }

{as}    { return AS;    }
{with}  { return WITH;  }
{using} { return USING; }
{split} { return SPLIT; }
{on}    { return ON;    }
{merge} { return MERGE; }

{builtin}      { yylval = strdup(yytext); return BUILTIN; }
{group}        { yylval = strdup(yytext); return GROUP;   }
<INITIAL>{str} { yylval = strdup(yytext); return STR;     }
{var}          { yylval = strdup(yytext); return VAR;     }
{dbl}          { yylval = strdup(yytext); return DBL;     }
{int}          { yylval = strdup(yytext); return INT;     }

{dep}     { return DEP; }
{eos}     { return EOS;       }
{compose} { return COMPOSE;   }
{equal}   { return EQUAL;     }
{couple}  { return COUPLE;    }
{label}   { return LABEL;     }
{cond}    { return CONDITION; }

{lbrk} { return LBRK; }
{rbrk} { return RBRK; }
{lpar} { return LPAR; }
{rpar} { return RPAR; }
{sep}  { return SEP;  }

%%

int newfile(char *fn){
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
