from grammar.base_grammar import Grammar

class RGrammar(Grammar):
    def __init__(
        self,
        source,
        manifolds,
        outdir,
        home
    ):
        self.source    = source
        self.manifolds = manifolds
        self.outdir    = outdir
        self.home      = home
        self.TRUE      = "TRUE"
        self.FALSE     = "FALSE"
        self.lang      = "R"
        self.INDENT = 2
        self.SEP    = ', '
        self.BIND   = '='
        self.AND    = ' && '
        self.LIST   = 'list({values})'
        self.POOL   = '''\
#!/usr/bin/Rscript --vanilla
library(readr)

outdir <- "{outdir}"

{type_map}

{source}

{manifolds}

{nat2uni}

{uni2nat}

args <- commandArgs(TRUE)
m <- args[1]

if(exists(m)){{
  cmd = paste0("show_", m)
  f <- get(cmd)
  d <- do.call(f, as.list(args[-1]))
  if(!is.null(d)){{
    result <- tryCatch({{
        write(d, file="")
      }}, warning = function(w) {{
        write("null", file="")
      }}, error = function(e) {{
        write("null", file="")
      }}, finally = {{
      
      }}
    )
  }}
}} else {{
  quit(status=1)
}}'''
        self.TYPE_MAP         = '''# skipping type map'''
        self.TYPE_MAP_PAIR    = "    {key}='{type}'"
        self.TYPE_ACCESS      = '''types["{key}"]'''
#        self.CAST_NAT2UNI     = '''natural_to_universal({key}, {type})'''
#        self.CAST_UNI2NAT     = '''universal_to_natural({key}, {type})'''
        self.CAST_NAT2UNI     = '''{key}'''
        self.CAST_UNI2NAT     = '''{key}'''
        self.NATIVE_MANIFOLD = '''\
{mid} = function ({marg_uid}){{
# {comment}
{blk}
}}
'''
        self.NATIVE_MANIFOLD_BLK = '''\
{hook0}
{cache}
{hook1}
return(b)\
'''
        self.SIMPLE_MANIFOLD = '''\
{mid} = function ({marg_uid}){{
# {comment}
{blk}
}}
'''
        self.SIMPLE_MANIFOLD_BLK = '''\
{function}({arguments})\
'''
        self.UID_WRAPPER = '''\
{mid}_uid = 0
wrap_{mid} <- function(...){{
{blk}
}}
'''
        self.UID_WRAPPER_BLK = '''\
{mid}_uid <<- {mid}_uid + 1
{mid} (..., uid={mid}_uid )\
'''
        self.UID = 'uid'
        self.MARG_UID = '{marg}, {uid}'
        self.WRAPPER_NAME = 'wrap_{mid}'
        self.FOREIGN_MANIFOLD = '''\
{mid} <- function({marg_uid}){{
# {comment}
{blk}
}}
'''
        self.FOREIGN_MANIFOLD_BLK = '''\
foreign_pool <- file.path(outdir, "call.{foreign_lang}")
fo <- "null"
fo <- tryCatch({{
    system2(
      foreign_pool,
      args=c({args}),
      stdout=TRUE, # capture STDOUT
      stderr=""    # let STDERR pass
    )
  }}, warning = function(w) {{
    "null" 
  }}, error = function(e) {{
    "null" 
  }}, finally = {{
  
  }}
)

read_{mid}(fo)\
'''
        self.CACHE = '''\
if({cache}_chk("{mid}"{uid}{cache_args})){{
{if_blk}
}}
else{{
{else_blk}
}}
'''
        self.CACHE_IF = '''\
{hook8}
b <- {cache}_get("{mid}"{uid}{cache_args})
{hook9}
'''
        self.CACHE_ELSE = '''\
{hook2}
{validate}
{hook3}\
'''
        self.DATCACHE_ARGS = '''outdir=outdir'''
        self.DO_VALIDATE = '''\
if( {checks} ){{
{if_blk}
}} else {{
{else_blk}
}}
'''
        self.RUN_BLK = '''\
{hook4}
b <- {function}({arguments})
{cache_put}
{hook5}
'''
        self.RUN_BLK_VOID = '''\
{hook4}
# force evaluation
b <- {function}({arguments})
# throw away the result
b <- NULL
{cache_put}
{hook5}
'''
        self.FAIL_BLK = '''\
{hook6}
b <- {fail}
{cache_put}
{hook7}
'''
        self.FAIL_BLK_VOID = '''\
{hook6}
{fail}
b <- NULL
{cache_put}
{hook7}
'''
        self.FAIL = '{fail}({marg_uid})'
        self.DEFAULT_FAIL = 'NULL'
        self.NO_VALIDATE = '''\
{hook4}
b = {function}({arguments})
{cache_put}
{hook5}
'''
        self.CACHE_PUT = '''\
{cache}_put("{mid}", b{other_args})
'''
        self.MARG          = 'x{i}'
        self.ARGUMENTS     = '{inputs}{sep}{fargs}'
        self.MANIFOLD_CALL = '{hmid}({marg_uid})'
        self.CHECK_CALL    = '{hmid}({marg_uid})'
        self.HOOK          = '''\
# {comment}
{hmid}({marg_uid})\
'''
