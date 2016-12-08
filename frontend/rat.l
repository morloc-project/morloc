%{
    #include "rat.tab.h"
%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap

comment #.*
ws      [ \t\n\r]+
int     [0-9]|[1-9][0-9]+
dbl     {int}\.[0-9]+
str     \'[^']*\'|\"[^"]*\"
var     [a-zA-Z_][a-zA-Z_0-9]
dep     -->
eol     [\r\n]+

compose \.
eos     ;|---+
equal   =
couple  {ws}::{ws}
label   :

lbrk    \[
rbrk    \]
lpar    \(
rpar    \)
sep     ,

builtin (memcache|datcache|nocache|true|null|id|call)

with  ^with{ws}|{ws}with{ws}
split ^split{ws}|{ws}split{ws}
on    ^on{ws}|{ws}on{ws}
merge ^merge{ws}|{ws}merge{ws}
using ^using{ws}|{ws}using{ws}

group   __all__


section_run     ^run:\n
section_compose ^compose:\n
section_alias   ^alias:\n
section_arg     ^arg:\n
section_cache   ^cache:\n
section_check   ^check:\n
section_effect  ^effect:\n
section_pack    ^pack:\n
section_open    ^open:\n
section_fail    ^fail:\n
section_pass    ^pass:\n
section_loop    ^loop:\n

%%


{comment} { }

{section_run}      { return SECTION_RUN;     }
{section_compose}  { return SECTION_COMPOSE; }
{section_alias}    { return SECTION_ALIAS;   }
{section_arg}      { return SECTION_ARG;     }
{section_cache}    { return SECTION_CACHE;   }
{section_check}    { return SECTION_CHECK;   }
{section_effect}   { return SECTION_EFFECT;  }
{section_pack}     { return SECTION_PACK;    }
{section_open}     { return SECTION_OPEN;    }
{section_fail}     { return SECTION_FAIL;    }
{section_pass}     { return SECTION_PASS;    }
{section_loop}     { return SECTION_LOOP;    }

{with}  { return WITH; }
{using} { return USING; }
{split} { return SPLIT; }
{on}    { return ON; }
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

{ws} { }
