from collections import namedtuple

Grammar = namedtuple("Grammar", [
    "SEP",
    "BIND",
    "AND",
    "POOL",
    "NATIVE_MANIFOLD",
    "FOREIGN_MANIFOLD",
    "CACHE",
    "DATCACHE_ARGS",
    "PROCESS",
    "DO_VALIDATE",
    "NO_VALIDATE",
    "ARGUMENTS",
    "MANIFOLD_CALL",
    "CHECK_CALL",
    "HOOK",
    "INDENT",
    "CACHE_PUT",
    "MARG",
    "LIST",
    "FAIL",
    "DEFAULT_FAIL",
    "UID_WRAPPER",
    "UID",
    "MARG_UID",
    "WRAPPER_NAME"
])

grammar = {}

# === R grammar ============================================

grammar['R'] = Grammar(
    INDENT = 2                ,
    SEP    = ', '             ,
    BIND   = '='              ,
    AND    = ' && '           ,
    LIST   = 'list({values})' ,
    POOL   = '''\
#!/usr/bin/Rscript --vanilla
library(readr)

# outdir={outdir}

{source}

{manifolds}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){{
  f = get(m)
  d <- f()
  if(is.data.frame(d)){{
      write_tsv(d, path="/dev/stdout")
  }} else {{
      write_lines(d, path="/dev/stdout")
  }}
}} else {{
  quit(status=1)
}}''',
    NATIVE_MANIFOLD = '''\
{mid} = function ({marg_uid}){{
  {hook0}
{block}
  {hook1}
  return(b)
}}
''',
    UID_WRAPPER = '''\
{mid}_uid = 0
wrap_{mid} <- function( {marg} ){{
    {mid}_uid <<- {mid}_uid + 1 
    uid <- {mid}_uid
    {mid} ( {marg_uid} )
}}
''',
    UID = 'uid',
    MARG_UID = '{marg}, {uid}',
    WRAPPER_NAME = 'wrap_{mid}',
    FOREIGN_MANIFOLD = '''\
{mid} <- function({marg_uid}){{
  d <- system("{outdir}/call.{foreign_lang} {mid} {marg_uid}", intern = TRUE)
  d <- read_tsv(d)
  if(ncol(d) == 1){{
    d <- d[[1]]
  }}
  return(d)
}}
''',
    CACHE = '''\
if({cache}_chk("{mid}"{uid}{cache_args})){{
  {hook8}
  b = {cache}_get("{mid}"{uid}{cache_args})
  {hook9}
}}
else{{
{process}
}}
''',
    DATCACHE_ARGS = '''outdir="{outdir}"''',
    PROCESS = '''\
{hook2}
{validate}
{hook3}
''',
    DO_VALIDATE = '''\
if( {checks} ){{
  {hook4}
  b = {function}({arguments})
  {cache_put}
  {hook5}
}} else {{
  {hook6}
  b = {fail}
  {cache_put}
  {hook7}
}}
''',
    FAIL = '{fail}({marg_uid})',
    DEFAULT_FAIL = 'NULL',
    NO_VALIDATE = '''\
{hook4}
b = {function}({arguments})
{cache_put}
{hook5}
''',
    CACHE_PUT = '''\
{cache}_put("{mid}", b{uid}{cache_args})
''',
    MARG          = 'x{i}',
    ARGUMENTS     = '{inputs}{sep}{fargs}',
    MANIFOLD_CALL = '{hmid}({marg_uid})',
    CHECK_CALL    = '{hmid}({marg_uid})',
    HOOK          = '{hmid}({marg_uid})'
)


# === sh grammar ===========================================

grammar['sh'] = Grammar(
    INDENT = 4,
    SEP    = ' ',
    BIND   = ' ',
    AND    = ' && ',
    LIST   = ' {value} ',
    POOL   = '''\
#!/usr/bin/env bash

outdir=$PWD/{outdir}

{source}

to_stderr () {{
    $@ 1>&2
}}

{manifolds}

manifold_exists() {{
    type $1 | grep -q function
}}
if manifold_exists $1
then
    if [[ -f $outdir/$1_tmp ]]
    then
        cat $outdir/$1_tmp
    else
        $@
    fi
else
    exit 1 
fi
''',
    NATIVE_MANIFOLD = '''\
{mid} () {{
    {hook0}
{block}
    {hook1}
}}
''',
    UID_WRAPPER  = '''\
{mid}_uid=0
wrap_{mid} () {{
    {mid}_uid=$(( {mid}_uid + 1 ))
    uid=${mid}_uid
    {mid} $1 {marg_uid}
}}
''',
    UID          = '${mid}_uid',
    MARG_UID     = '{marg} {uid}',
    WRAPPER_NAME = 'wrap_{mid}',
    FOREIGN_MANIFOLD = '''\
{mid} () {{
    {outdir}/call.{foreign_lang} {mid} {marg_uid}
}}
''',
    CACHE = '''\
if {cache}_chk {mid}{uid}
then
    {hook8}
    {cache}_get {mid}{uid}
    {hook9}
else
    {process}
fi
''',
    DATCACHE_ARGS = "",
    PROCESS = '''\
{hook2}
{validate}
{hook3}
( cat $outdir/{mid}_tmp ; rm $outdir/{mid}_tmp )
''',
    DO_VALIDATE = '''\
if {checks}
then
    {hook4}
    {function} {arguments} > $outdir/{mid}_tmp
    {cache_put}
    {hook5}
else
    {hook6}
    {fail}> $outdir/{mid}_tmp
    {cache_put}
    {hook7}
fi
''',
    FAIL = '''{fail} {marg_uid} ''',
    DEFAULT_FAIL = "",
    NO_VALIDATE = '''\
{hook4}
{function} {arguments} > $outdir/{mid}_tmp
{cache_put}
{hook5}
''',
    CACHE_PUT = '''\
{cache}_put {mid} $outdir/{mid}_tmp {uid} {cache_args}
''',
    MARG          = '${i}',
    ARGUMENTS     = '{fargs} {inputs}',
    MANIFOLD_CALL = '<({hmid} {marg_uid})',
    CHECK_CALL    = '{hmid} {marg_uid}',
    HOOK          = 'to_stderr {hmid} {marg_uid}'
)
