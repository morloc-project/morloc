%{
    #include "rat.tab.h"
%}

%option outfile="lex.yy.c" header-file="lex.yy.h"

%option noyywrap

comment #.*
ws      [ \t\n\r]+
val     [a-zA-Z0-9_]*
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

%%


{comment} { }

^run:\n     { return SECTION_RUN;     }
^compose:\n { return SECTION_COMPOSE; }
^alias:\n   { return SECTION_ALIAS;   }
^arg:\n     { return SECTION_ARG;     }
^cache:\n   { return SECTION_CACHE;   }
^check:\n   { return SECTION_CHECK;   }
^effect:\n  { return SECTION_EFFECT;  }
^pack:\n    { return SECTION_PACK;    }
^open:\n    { return SECTION_OPEN;    }
^fail:\n    { return SECTION_FAIL;    }
^pass:\n    { return SECTION_PASS;    }
^loop:\n    { return SECTION_LOOP;    }

{with}  { return WITH; }
{using} { return USING; }
{split} { return SPLIT; }
{on}    { return ON; }
{merge} { return MERGE; }

{builtin} { yylval = strdup(yytext); return BUILTIN; }
{group}   { yylval = strdup(yytext); return GROUP;   }
{val}     { yylval = strdup(yytext); return VAL;     }
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
