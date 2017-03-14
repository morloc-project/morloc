from mogrifier.base_mogrifier import Mogrifier

uni2nat_top = '''
require(readr)
'''

universal_to_atom = {
    "Int"    : "as.integer({x})",
    "Num"    : "as.numeric({x})",
    "String" : "as.character({x})",
    "File"   : "as.character({x})",
    "Bool"   : "switch(x, true=TRUE, false=FALSE, NA)",
    "Text"   : "read_lines({x})",
    "void"   : "NULL"
}

nat2uni_top = '''
suppressPackageStartupMessages(library("jsonlite"))

get_tmpfile <- function(){
    tempfile(pattern="R_", tmpdir=outdir, fileext=".out")
}

as_file <- function(x){
    f <- get_tmp_file()
    write_lines(x, file=f)
    f
}
'''

atom_to_universal = {
    "Int"    : "{x}",
    "Num"    : "{x}",
    "String" : "{x}",
    "File"   : "{x}",
    "Bool"   : "if({x}){{ 'true' }} else {{ 'false' }}",
    "Text"   : "as_file({x})",
    "void"   : "NULL"
}

universal_to_natural = '''
read_{{mid}} <- function(x){{{{
    # {comment}
    {cast}
}}}}
'''

natural_to_universal = '''
show_{{mid}} <- function(...){{{{
    # {comment}
    {cast}
}}}}
'''

class RMogrifier(Mogrifier):
    def __init__(self, manifolds):

        super().__init__(manifolds)

        self.manifolds = manifolds

        # key: name of atomic type
        # val: a function that maps between universal and native
        self.universal_to_atom = universal_to_atom
        self.atom_to_universal = atom_to_universal

        self.uni2nat_top = uni2nat_top
        self.nat2uni_top = nat2uni_top

        # templates for conversion functions
        self.universal_to_natural = universal_to_natural
        self.natural_to_universal = natural_to_universal

    def _universal_to_primitive(self, typ):
        s = 'if(length(x) == 0){{ "null" }} else {{ fromJSON(x, simplifyVector=TRUE) }}'
        return s

    def _universal_to_tuple(self, typ):
        s = 'if(length(x) == 0){{ "null" }} else {{ fromJSON(x, simplifyVector=FALSE) }}'
        return s

    def _universal_to_array(self, typ):
        s = 'if(length(x) == 0){{ "null" }} else {{ fromJSON(x, simplifyVector=TRUE) }}'
        return s

    def _universal_to_wtf(self, typ):
        s = 'if(length(x) == 0){{ "null" }} else {{ fromJSON(x, simplifyVector=TRUE) }}'
        return s

    def _wtf_to_universal(self, typ):
        s = 'toJSON({mid}(...), auto_unbox=TRUE, null="null", na="null")'
        return s

    def _primitive_to_universal(self, typ):
        s = 'toJSON({mid}(...), auto_unbox=TRUE, null="null", na="null")'
        return s

    def _tuple_to_universal(self, typ):
        s = 'toJSON({mid}(...), auto_unbox=TRUE, null="null", na="null")'
        return s

    def _array_to_universal(self, typ):
        s = 'toJSON({mid}(...), auto_unbox=TRUE, null="null", na="null")'
        return s
