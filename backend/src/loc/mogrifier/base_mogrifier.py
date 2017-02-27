from util import err,indent,clean
import sys

# The PEG grammar parser for parsing types
from arpeggio import *
from arpeggio import RegExMatch as _

def typeIdent(): return _('[A-Za-z0-9_]+')
def typeTuple(): return '(', typeExpr, OneOrMore(',', typeExpr), ')'
def typeArray(): return '[', typeExpr, ']'
def typeFunc():  return '(', typeExpr, OneOrMore('->', typeExpr), ')'
def typeExpr():  return [typeIdent, typeTuple, typeArray, typeFunc]
def typeType():  return typeExpr, EOF

class typeTypeVisitor(PTNodeVisitor):

    def __init__(
        self,      # boiplate
        mog,       # an object holding all required template strings
        direction  # ["uni2nat", "nat2uni"]
    ):
        super().__init__()
        self.mog = mog
        self.direction = direction

    def visit_typeIdent(self, node, children):
        # TODO I am given type, but need to produce a nested function that takes 'x'
        # TODO need to extend this elsewhere
        ident = node.value
        if ident in self.mog.universal_to_atom.keys():
            return(self.mog.universal_to_atom[ident] % "x")

    def visit_typeTuple(self, node, children):
        # TODO replace
        return("tuple(%s)" % ','.join(children))

    def visit_typeArray(self, node, children):
        # TODO replace
        return("array(%s)" % children[0])

    def visit_typeFunc(self, node, children):
        # TODO replace
        return("function(%s)" % ','.join(children))

    def visit_typeExpr(self, node, children):
        # TODO replace
        return(children[0])

    def visit_typeType(self, node, children):
        # TODO replace
        return(children[0])

class Mogrifier:
    def __init__(self, manifolds):
        self.manifolds = manifolds

        # key: name of atomic type
        # val: a function that maps between universal and native
        self.universal_to_atom = dict()
        self.atom_to_universal = dict()

        # any code that required for the mogrifier
        self.uni2nat_top = "" 
        self.nat2uni_top = ""

        # templates for conversion functions
        self.universal_to_natural = None
        self.natural_to_universal = None

        self.parser = ParserPython(typeType)

        #  # TODO: this needs to go wherever the type is processed 
        #  parse_tree = parser.parse(t)
        #
        #  result = visit_parse_tree(parse_tree, typeTypeVisitor())

    def build_uni2nat(self):
        out = [self.uni2nat_top]
        for m in self.manifolds.values():
            function_name = "read_" + m.mid
            s = self.universal_to_natural.format(
                name=function_name
            )
            out.append(s)
        return '\n'.join(out)

    def build_nat2uni(self):
        out = [self.nat2uni_top]
        for m in self.manifolds.values():
            function_name = "show_" + m.mid
            s = self.natural_to_universal.format(
                name=function_name
            )
            out.append(s)
        return '\n'.join(out)

    def _parse_type(self):
        raise NotImplemented

    def _category(self):
        raise NotImplemented

    def _universal_to_primitive(self, typ):
        raise NotImplemented

    def _primitive_to_universal(self, typ):
        raise NotImplemented

    def _universal_to_primitive_tuple(self, typ):
        raise NotImplemented

    def _primitive_tuple_to_universal(self, typ):
        raise NotImplemented

    def _tuple_to_universal(self, typ):
        raise NotImplemented

    def _universal_to_tuple(self, typ):
        raise NotImplemented

    def _universal_to_array(self, typ):
        raise NotImplemented

    def _array_to_universal(self, typ):
        raise NotImplemented
