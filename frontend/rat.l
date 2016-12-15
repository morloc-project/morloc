%{
    #include "rat.tab.h"

/* My buffer stack handling code draws heavily from: */
/* John Levine (2009) 'Flex and Bison'               */
typedef struct bufstack{
    struct bufstack *prev;    
    YY_BUFFER_STATE bs;
    int lineno;
    char *filename;
    FILE *f;
} bufstack;

/* global variable for storing filename */
char* current_filename = NULL;

/* global variable for storing buffer stack */
bufstack *current_bs = NULL;

/* descend into an included file */ 
int newfile(char *fn);

/* leave a file */
int popfile(void);

FILE* toklog;

#define LOG(x) fprintf(toklog, x)
#define LOGV(x,a) fprintf(toklog, x, a)

%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap pointer yylineno

%s S_IMPORT
%x S_LANG
%x S_SOURCE
%x S_COMMENT

line    ^(\n|[^@]).*
comment #.*\n
ws      [ \t\n\r]+
not_ws  [^ \t\n\r]+
int     [0-9]|[1-9][0-9]+
dbl     {int}\.[0-9]+
str     \'[^']*\'|\"[^"]*\"
var     [a-zA-Z_][a-zA-Z_0-9]*

pos     \$[1-9][0-9]*    
cond    \?{ws}*{var}

compose "."
eos     ;|---+
equal   =
couple  ::
label   :
rarrow  "->"
deref   &

lbrk    \[
rbrk    \]
lpar    \(
rpar    \)
bar     \|
sep     ,

builtin (memcache|datcache|nocache|true|null|id|call)
logical (TRUE|NULL|FALSE)

group   __all__

section_action   @action
section_alias    @alias
section_arg      @arg
section_cache    @cache
section_check    @check
section_comment  @comment
section_doc      @doc
section_effect   @effect
section_export   @export
section_fail     @fail
section_import   @import
section_ontology @ontology
section_open     @open
section_pack     @pack
section_pass     @pass
section_path     @path
section_source   @source
section_type     @type



%%
{comment} { }

{section_comment}  { LOG(""                 ); BEGIN S_COMMENT; }
{section_import}   { LOG("SECTION_IMPORT"   ); BEGIN S_IMPORT;  }
{section_ontology} { LOG("SECTION_ONTOLOGY" ); BEGIN INITIAL;   return SECTION_ONTOLOGY; }
{section_source}   { LOG("SECTION_SOURCE"   ); BEGIN S_LANG;    return SECTION_SOURCE;   }
{section_type}     { LOG("SECTION_TYPE"     ); BEGIN INITIAL;   return SECTION_TYPE;     }
{section_doc}      { LOG("SECTION_DOC"      ); BEGIN INITIAL;   return SECTION_DOC;      }
{section_export}   { LOG("SECTION_EXPORT"   ); BEGIN INITIAL;   return SECTION_EXPORT;   }
{section_path}     { LOG("SECTION_PATH"     ); BEGIN INITIAL;   return SECTION_PATH;     }
{section_alias}    { LOG("SECTION_ALIAS"    ); BEGIN INITIAL;   return SECTION_ALIAS;    }
{section_arg}      { LOG("SECTION_ARG"      ); BEGIN INITIAL;   return SECTION_ARG;      }
{section_cache}    { LOG("SECTION_CACHE"    ); BEGIN INITIAL;   return SECTION_CACHE;    }
{section_check}    { LOG("SECTION_CHECK"    ); BEGIN INITIAL;   return SECTION_CHECK;    }
{section_effect}   { LOG("SECTION_EFFECT"   ); BEGIN INITIAL;   return SECTION_EFFECT;   }
{section_pack}     { LOG("SECTION_PACK"     ); BEGIN INITIAL;   return SECTION_PACK;     }
{section_open}     { LOG("SECTION_OPEN"     ); BEGIN INITIAL;   return SECTION_OPEN;     }
{section_fail}     { LOG("SECTION_FAIL"     ); BEGIN INITIAL;   return SECTION_FAIL;     }
{section_pass}     { LOG("SECTION_PASS"     ); BEGIN INITIAL;   return SECTION_PASS;     }

<S_COMMENT>@      { unput('@'); BEGIN INITIAL; }
<S_COMMENT>(.|\n) { /* toss bodies */ }
<S_COMMENT>\\@    { /* allow escaped @ */ }

<S_LANG>[ ]+{var} {
    LOG(" LANG");
    BEGIN S_SOURCE;
    while(yytext[0] == ' ' || yytext[0] == '\t' || yytext[0] == '\n') yytext++;
    return LANG;
  }
<S_SOURCE>^[ \t]*([^\n@].*)?\n { LOGV("%s", yytext); return LINE; }
<S_SOURCE>@ { unput('@'); BEGIN INITIAL; } /* allow @ anywhere except ^ */

<S_IMPORT>{str} {
    yytext++;
    yytext[strlen(yytext)-1]='\0';
    if(!newfile(yytext)) yyterminate();
  }
<<EOF>>         { if(!popfile()) yyterminate(); }

{cond} {
    /* remove ? */
    yytext++;
    /* remove whitespace */
    while(yytext[0] == ' ' || yytext[0] == '\t' || yytext[0] == '\n') yytext++;
    LOGV("COND-%s ", yytext);
    return COND;
  }
{pos} {
    yytext++; /* remove $ */
    LOGV("POS-%s", yytext);
    return POS;
}

AS        { LOG("AS "      ); return AS;      }
{rarrow}  { LOG("RARR "    ); return RARR;    }
{bar}     { LOG("BAR "     ); return BAR;     }
{deref}   { LOG("DEREF "   ); return DEREF;   }
{builtin} { LOG("BUILTIN " ); return BUILTIN; }
{group}   { LOG("GROUP "   ); return GROUP;   }
{str}     { LOG("STR "     ); return STR;     }
{var}     { LOG("VAR "     ); return VAR;     }
{dbl}     { LOG("DBL "     ); return DBL;     }
{int}     { LOG("INT "     ); return INT;     }
{eos}     { LOG("EOS "     ); return EOS;     }
{compose} { LOG("COMPOSE " ); return COMPOSE; }
{equal}   { LOG("EQUAL "   ); return EQUAL;   }
{couple}  { LOG("COUPLE "  ); return COUPLE;  }
{label}   { LOG("LABEL "   ); return LABEL;   }
{lbrk}    { LOG("LBRK "    ); return LBRK;    }
{rbrk}    { LOG("RBRK "    ); return RBRK;    }
{lpar}    { LOG("LPAR "    ); return LPAR;    }
{rpar}    { LOG("RPAR "    ); return RPAR;    }
{sep}     { LOG("SEP "     ); return SEP;     }

{ws}      { LOG(yytext); }
.         { LOG(yytext); }

%%

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
