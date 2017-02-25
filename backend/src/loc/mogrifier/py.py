from mogrifier.base_mogrifier import Mogrifier

universal_to_atom = {
    "String" : "str({x})",
    "File"   : "str({x})",
    "Int"    : "int({x})",
    "Num"    : "float({x})",
    "Bool"   : "bool({x})",
    "*"      : "str({x})",
    "Text"   : "read_text({x})",
    "void"   : "None"
}

atom_to_universal = {
    "String" : "str({x})",
    "File"   : "str({x})",
    "Int"    : "str({x})",
    "Num"    : "str({x})",
    "Bool"   : "str(int({x}))",
    "*"      : "str({x})",
    "Text"   : "write_text({x})",
    "void"   : "None"
}

uni2nat_top = '''
def read_text(filename):
    raise NotImplemented
'''

nat2uni_top = '''
def write_text(x):
    raise NotImplemented
'''

universal_to_natural = '''
def {name}(x):
    return x
'''

natural_to_universal = '''
def {name}(x):
    return x
'''


class PyMogrifier(Mogrifier):
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
