%{
#include <ctype.h>
#include "rat.tab.h"

int newfile(char *fn);
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
%s S_COMPOSITION
%s S_ASSIGNMENT

line    ^(\n|[^@]).*
comment #.*\n
ws      [ \t\n\r]+
not_ws  [^ \t\n\r]+
int     [0-9]|[1-9][0-9]+
dbl     {int}\.[0-9]+
str     \'[^']*\'|\"[^"]*\"
var     [a-zA-Z_][a-zA-Z_0-9]*
ident   {var}(\/{var})*[ \t\n\r]*(::|=|{ws}as{ws})

positional \$[1-9][0-9]*
groupref   \*{var}
cond       \?{ws}*{var}

eos     ;|---+
couple  ::
label   :
rarrow  "->"
deref   &

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

{section_comment}  { LOG(""                 ); BEGIN S_COMMENT;     }
{section_import}   { LOG("SECTION_IMPORT"   ); BEGIN S_IMPORT;      }
{section_ontology} { LOG("SECTION_ONTOLOGY" ); BEGIN INITIAL;       return SECTION_ONTOLOGY; }
{section_source}   { LOG("SECTION_SOURCE"   ); BEGIN S_LANG;        return SECTION_SOURCE;   }
{section_type}     { LOG("SECTION_TYPE"     ); BEGIN INITIAL;       return SECTION_TYPE;     }
{section_doc}      { LOG("SECTION_DOC"      ); BEGIN INITIAL;       return SECTION_DOC;      }
{section_export}   { LOG("SECTION_EXPORT"   ); BEGIN INITIAL;       return SECTION_EXPORT;   }
{section_path}     { LOG("SECTION_PATH"     ); BEGIN S_COMPOSITION; return SECTION_PATH;     }
{section_check}    { LOG("SECTION_CHECK"    ); BEGIN S_COMPOSITION; return SECTION_CHECK;    }
{section_effect}   { LOG("SECTION_EFFECT"   ); BEGIN S_COMPOSITION; return SECTION_EFFECT;   }
{section_alias}    { LOG("SECTION_ALIAS"    ); BEGIN INITIAL;       return SECTION_ALIAS;    }
{section_arg}      { LOG("SECTION_ARG"      ); BEGIN INITIAL;       return SECTION_ARG;      }
{section_cache}    { LOG("SECTION_CACHE"    ); BEGIN INITIAL;       return SECTION_CACHE;    }
{section_pack}     { LOG("SECTION_PACK"     ); BEGIN INITIAL;       return SECTION_PACK;     }
{section_open}     { LOG("SECTION_OPEN"     ); BEGIN INITIAL;       return SECTION_OPEN;     }
{section_fail}     { LOG("SECTION_FAIL"     ); BEGIN INITIAL;       return SECTION_FAIL;     }
{section_pass}     { LOG("SECTION_PASS"     ); BEGIN INITIAL;       return SECTION_PASS;     }

<S_COMMENT>@      { unput('@'); BEGIN INITIAL; }
<S_COMMENT>(.|\n) { /* toss bodies */ }
<S_COMMENT>\\@    { /* allow escaped @ */ }

<S_LANG>[ ]+{var} {
    LOG(" LANG");
    BEGIN S_SOURCE;
    while(isspace(yytext[0])) yytext++;
    yylval.LANG = strdup(yytext);
    return LANG;
  }
<S_SOURCE>^[ \t]*([^\n@].*)?\n {
    LOGV("%s", yytext);
    yytext[strlen(yytext)-1] = '\0'; /* trim the newline */ 
    yylval.LINE = strdup(yytext);
    return LINE;
}
<S_SOURCE>@ { unput('@'); BEGIN INITIAL; } /* allow @ anywhere except ^ */

<S_IMPORT>{str} {
    yytext++;
    yytext[strlen(yytext)-1]='\0';
    if(!newfile(yytext)) yyterminate();
  }
<<EOF>> { if(!popfile()) yyterminate(); }

as        { LOG("AS "         ); return AS;         }
{logical} { LOG("LOG "        ); return LOG;        }
{rarrow}  { LOG("RARR "       ); return RARR;       }
{deref}   { LOG("DEREF "      ); }

{str} {
    LOG("STR ");
    char* s = strdup(yytext);
    s++; s[strlen(s)-1] = '\0'; /* unquote */
    yylval.STR = s;
    return STR;
}

<S_COMPOSITION>{cond} {
    /* remove initial '?' */
    yytext++;
    /* remove whitespace */
    while(isspace(yytext[0])) yytext++;
    LOGV("COMPOSON.CONDITIONAL-%s ", yytext);
    yylval.COMPOSON = new_Composon(C_CONDITIONAL);
    yylval.COMPOSON->value.name = strdup(yytext);
    return COMPOSON;
}

<S_COMPOSITION>{positional} {
    yytext++; /* remove '$' */
    LOGV("COMPOSON.C_POSITIONAL-%s", yytext);
    yylval.COMPOSON = new_Composon(C_POSITIONAL);
    yylval.COMPOSON->value.name = strdup(yytext);
    return COMPOSON;
}

<S_COMPOSITION>{groupref} {
    yytext++; /* remove '*' */
    yylval.COMPOSON = new_Composon(C_GROUP);
    yylval.COMPOSON->value.name = strdup(yytext);
    LOGV("GROUP-%s", yylval.COMPOSON->value.name);
    return COMPOSON;
}

<S_COMPOSITION>{var} {
    LOG("COMPOSON ");
    yylval.COMPOSON = new_Composon(C_VARIABLE);
    yylval.COMPOSON->value.name = strdup(yytext);
    yylval.COMPOSON->manifold = new_Manifold(strdup(yytext));
    return COMPOSON;
}

{var} {
    LOG("VARIABLE ");
    yylval.VARIABLE = strdup(yytext);
    return VARIABLE;
}

{ident}   {
    char* s = strdup(yytext);
    int i = 0;
    for(;;i++){
        char c = s[i];
        if(c == '/' || isspace(c) || c == ':' || c == '=')
            break;
    }
    for(int j = strlen(s) - 1; j >= i ; j--){
        unput(s[j]);
    }
    s[i] = '\0';
    yylval.IDENTIFIER = new_Label(s);
    LOGV("IDENTIFIER-%s ", s);

    return IDENTIFIER;
}

{dbl}     { LOG("DBL "        ); return DBL;        }
{int}     { LOG("INT "        ); return INT;        }
{eos}     { LOG("EOS "        ); }
{couple}  { LOG("COUPLE "     ); return COUPLE;     }
{label}   { LOG("LABEL "      ); }

{ws}      { LOG(yytext); }
.         { LOG(yytext); return yytext[0]; }

%%

#include "bufstack.h"

int main(int argc, char ** argv){
    if(argc < 2){
        perror("Please provide a filename\n");
        return 1;
    }
    if(newfile(argv[1]))
        toklog = fopen("tok.log", "w");
        yyparse();
    return 0;
}
