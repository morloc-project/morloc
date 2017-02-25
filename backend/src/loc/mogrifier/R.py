from mogrifier.base_mogrifier import Mogrifier

uni2nat_top = '''
require(readr)
'''

universal_to_atom = {
    "Int"    : "as.integer({x})",
    "Num"    : "as.numeric({x})",
    "String" : "as.character({x})",
    "File"   : "as.character({x})",
    "Bool"   : "as.logical(as.integer({x}))",
    "Text"   : "read_lines({x})",
    "void"   : "NULL"
}

nat2uni_top = '''
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
    "Int"    : "as.character({x})",
    "Num"    : "as.character({x})",
    "String" : "{x}",
    "File"   : "{x}",
    "Bool"   : "as.integer({x})",
    "Text"   : "as_file({x})",
    "void"   : "NULL"
}

universal_to_natural = '''
{name} <- function(x){{
    x
}}
'''

natural_to_universal = '''
{name} <- function(x){{
    x
}}
'''

class RMogrifier(Mogrifier):
    def __init__(self, manifolds):

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
