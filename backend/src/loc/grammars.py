SEP              = {}
BIND             = {}
AND              = {}
POOL             = {}
NATIVE_MANIFOLD  = {}
FOREIGN_MANIFOLD = {}
CACHE            = {}
DATCACHE_ARGS    = {}
PROCESS          = {}
DO_VALIDATE      = {}
NO_VALIDATE      = {}
ARGUMENTS        = {}
MANIFOLD_CALL    = {}
CHECK_CALL       = {}
HOOK             = {}
INDENT           = {}
CACHE_PUT        = {}
MARG             = {}
LIST             = {}
FAIL             = {}
DEFAULT_FAIL     = {}
UID_WRAPPER      = {}
UID              = {}
MARG_UID         = {}
WRAPPER_NAME     = {}



# === R grammar ============================================

INDENT['R'] = 2
SEP['R'] = ', '
BIND['R'] = '='
AND['R'] = ' && '
LIST['R'] = 'list({values})'

POOL['R'] = '''\
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
}}'''

NATIVE_MANIFOLD['R'] = '''\
{uid_wrapper}
{mid} = function ({marg_uid}){{
  {hook0}
{block}
  {hook1}
  return(b)
}}
'''

UID_WRAPPER['R'] = '''\
{mid}_uid = 0
wrap_{mid} <- function( {deref} ){{
    {mid}_uid <<- {mid}_uid + 1 
    uid <- {mid}_uid
    {function} ( {marg}{sep}uid )
}}
'''
UID['R'] = 'uid'
MARG_UID['R'] = '{marg}, {uid}'
WRAPPER_NAME['R'] = 'wrap_{mid}'

FOREIGN_MANIFOLD['R'] = '''\
{mid} <- function({marg_uid}){{
  d <- system("{outdir}/call.{foreign_lang} {mid} {marg_uid}", intern = TRUE)
  d <- read_tsv(d)
  if(ncol(d) == 1){{
    d <- d[[1]]
  }}
  return(d)
}}
'''

CACHE['R'] = '''\
if({cache}_chk("{mid}"{uid}{cache_args})){{
  {hook8}
  b = {cache}_get("{mid}"{uid}{cache_args})
  {hook9}
}}
else{{
{process}
}}
'''

DATCACHE_ARGS['R'] = '''\
, outdir="{outdir}"'''

PROCESS['R'] = '''\
{hook2}
{validate}
{hook3}
'''

DO_VALIDATE['R'] = '''\
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
'''

FAIL['R'] = '{fail}({marg_uid})'
DEFAULT_FAIL['R'] = 'NULL'

NO_VALIDATE['R'] = '''\
{hook4}
b = {function}({arguments})
{cache_put}
{hook5}
'''

CACHE_PUT['R'] = '''\
{cache}_put("{mid}", b{uid}{cache_args})
'''

MARG['R']          = 'x{i}'
ARGUMENTS['R']     = '{inputs}{sep}{fargs}'
MANIFOLD_CALL['R'] = '{hmid}({marg_uid})'
CHECK_CALL['R']    = '{hmid}({marg_uid})'
HOOK['R']          = '{hmid}({marg_uid})'





# === sh grammar ===========================================

INDENT['sh'] = 4
SEP['sh'] = ' '
BIND['sh'] = ' '
AND['sh'] = ' && '
LIST['sh'] = ' {value} '

POOL['sh'] = '''\
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
'''

NATIVE_MANIFOLD['sh'] = '''\
{mid} ({marg_uid}) {{
    {hook0}
{block}
    {hook1}
}}
'''

UID_WRAPPER['sh'] = '''\
stub_wrapper
'''
UID['sh'] = 'stub_uid'
MARG_UID['sh'] = 'stub_marg_uid'
WRAPPER_NAME['sh'] = 'wrap_{mid}'

FOREIGN_MANIFOLD['sh'] = '''\
{mid} () {{
    {outdir}/call.{foreign_lang} {mid} {marg_uid}
}}
'''

CACHE['sh'] = '''\
if {cache}_chk {mid}{uid}
then
    {hook8}
    {cache}_get {mid}{uid}
    {hook9}
else
    {process}
fi
'''

DATCACHE_ARGS['sh'] = ""

PROCESS['sh'] = '''\
{hook2}
{validate}
{hook3}
( cat $outdir/{mid}_tmp ; rm $outdir/{mid}_tmp )
'''

DO_VALIDATE['sh'] = '''\
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
'''

FAIL['sh'] = '''{fail} {marg_uid} '''
DEFAULT_FAIL['sh'] = ""

NO_VALIDATE['sh'] = '''\
{hook4}
{function} {arguments} > $outdir/{mid}_tmp
{cache_put}
{hook5}
'''

CACHE_PUT['sh'] = '''\
{cache}_put {mid} $outdir/{mid}_tmp {uid} {cache_args}
'''

MARG['sh']          = '${i}'
ARGUMENTS['sh']     = '{fargs} {inputs}'
MANIFOLD_CALL['sh'] = '<({hmid} {marg_uid})'
CHECK_CALL['sh']    = '{hmid} {marg_uid}'
HOOK['sh']          = 'to_stderr {hmid} {marg_uid}'
