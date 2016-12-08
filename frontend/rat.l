%{
    #include "rat.tab.h"

/*
{comment} { printf( "COMMENT\n"); }
{eol} { printf("\n"); }
{ws} { }
{section_export}  { printf( "SECTION_EXPORT"  ); }
{section_import}  { printf( "SECTION_IMPORT"  ); }
{section_path}    { printf( "SECTION_PATH"    ); }
{section_compose} { printf( "SECTION_COMPOSE" ); }
{section_alias}   { printf( "SECTION_ALIAS"   ); }
{section_arg}     { printf( "SECTION_ARG"     ); }
{section_cache}   { printf( "SECTION_CACHE"   ); }
{section_check}   { printf( "SECTION_CHECK"   ); }
{section_effect}  { printf( "SECTION_EFFECT"  ); }
{section_pack}    { printf( "SECTION_PACK"    ); }
{section_open}    { printf( "SECTION_OPEN"    ); }
{section_fail}    { printf( "SECTION_FAIL"    ); }
{section_pass}    { printf( "SECTION_PASS"    ); }
{section_loop}    { printf( "SECTION_LOOP"    ); }
{with}            { printf( "WITH "           ); }
{using}           { printf( "USING "          ); }
{split}           { printf( "SPLIT "          ); }
{on}              { printf( "ON "             ); }
{as}              { printf( "AS "             ); }
{merge}           { printf( "MERGE "          ); }
{builtin}         { printf( "BUILTIN "        ); }
{group}           { printf( "GROUP "          ); }
{str}             { printf( "STR "            ); }
{var}             { printf( "VAR "            ); }
{dbl}             { printf( "DBL "            ); }
{int}             { printf( "INT "            ); }
{dep}             { printf( "DEP "            ); }
{eos}             { printf( "EOS "            ); }
{compose}         { printf( "COMPOSE "        ); }
{equal}           { printf( "EQUAL "          ); }
{couple}          { printf( "COUPLE "         ); }
{label}           { printf( "LABEL "          ); }
{lbrk}            { printf( "LBRK "           ); }
{rbrk}            { printf( "RBRK "           ); }
{lpar}            { printf( "LPAR "           ); }
{rpar}            { printf( "RPAR "           ); }
{sep}             { printf( "SEP "            ); }
*/

%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap

comment #.*\n
ws      [ \t\n\r]+
int     [0-9]|[1-9][0-9]+
dbl     {int}\.[0-9]+
str     \'[^']*\'|\"[^"]*\"
var     [a-zA-Z_][a-zA-Z_0-9]*
dep     -->
eol     [\n\r]+

compose \.
eos     ;|---+
equal   =
couple  ::
label   :

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

{section_export}  { return SECTION_EXPORT;  }
{section_import}  { return SECTION_IMPORT;  }
{section_path}    { return SECTION_PATH;    }
{section_compose} { return SECTION_COMPOSE; }
{section_alias}   { return SECTION_ALIAS;   }
{section_arg}     { return SECTION_ARG;     }
{section_cache}   { return SECTION_CACHE;   }
{section_check}   { return SECTION_CHECK;   }
{section_effect}  { return SECTION_EFFECT;  }
{section_pack}    { return SECTION_PACK;    }
{section_open}    { return SECTION_OPEN;    }
{section_fail}    { return SECTION_FAIL;    }
{section_pass}    { return SECTION_PASS;    }
{section_loop}    { return SECTION_LOOP;    }

{eos}   { return EOS;   }
{as}    { return AS;    }
{with}  { return WITH;  }
{using} { return USING; }
{split} { return SPLIT; }
{on}    { return ON;    }
{merge} { return MERGE; }

{builtin} { yylval = strdup(yytext); return BUILTIN; }
{group}   { yylval = strdup(yytext); return GROUP;   }
{str}     { yylval = strdup(yytext); return STR;     }
{var}     { yylval = strdup(yytext); return VAR;     }
{dbl}     { yylval = strdup(yytext); return DBL;     }
{int}     { yylval = strdup(yytext); return INT;     }

{dep}     { return DEP; }

{eos}     { return EOS;     }
{compose} { return COMPOSE; }
{equal}   { return EQUAL;   }
{couple}  { return COUPLE;  }
{label}   { return LABEL;   }

{lbrk} { return LBRK; }
{rbrk} { return RBRK; }
{lpar} { return LPAR; }
{rpar} { return RPAR; }
{sep}  { return SEP;  }
