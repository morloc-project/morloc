from util import err,indent,clean
import sys

# The PEG grammar parser for parsing types
from arpeggio import *
from arpeggio import RegExMatch as _


# =============================================================================
# --- Type parser
# Reads a Haskell style type definition. It reads a single type, not a type
# signature. The result is a tuple of form '(<constructor>, <value>)'. The
# constructor can currently be any of the following: "atomic", "tuple",
# "array", or "function". The value can be nested. For example,
#    ("tuple", (("atomic", "Int"), ("array", ("tuple", ("String", "String")))))
# which would correspond to
#    (Int, [(String, String)])
# This tuple is used to determine which conversion functions should be used in
# the output code. 
# =============================================================================

def typeIdent(): return _('\*|[A-Za-z0-9_]+|void')
def typeTuple(): return '(', typeExpr, OneOrMore(',', typeExpr), ')'
def typeArray(): return '[', typeExpr, ']'
def typeFunc():  return '(', typeExpr, OneOrMore('->', typeExpr), ')'
def typeExpr():  return [typeIdent, typeTuple, typeArray, typeFunc]
def typeType():  return typeExpr, EOF

class typeTypeVisitor(PTNodeVisitor):

    def visit_typeIdent(self, node, children):
        return(("atomic", node.value))

    def visit_typeTuple(self, node, children):
        return(("tuple", children))

    def visit_typeArray(self, node, children):
        return(("array", children[0]))

    def visit_typeFunc(self, node, children):
        return(("function", children))

    def visit_typeExpr(self, node, children):
        return(children[0])

    def visit_typeType(self, node, children):
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

    def _parse_type(self, otype):
        parse_tree = self.parser.parse(otype)
        type_tree = visit_parse_tree(parse_tree, typeTypeVisitor())
        return(type_tree)

    def nat2uni(self, tree):
        '''
        Generate a JSON file from a native data structure
        '''

        function = None

        if tree[0] == "atomic":
            function = self._primitive_to_universal(tree[1])
        elif tree[0] == "tuple":
            function = self._tuple_to_universal(tree[1])
        elif tree[0] == "array":
            function = self._array_to_universal(tree[1])
        else:
            print("Constructor '%s' is not supported" % str(tree), file=sys.stderr)

        return function

    def uni2nat(self, tree):
        '''
        Generate a native datastructure from a JSON file
        '''

        function = None

        if tree[0] == "atomic":
            function = self._universal_to_primitive(tree[1])
        elif tree[0] == "tuple":
            function = self._universal_to_tuple(tree[1])
        elif tree[0] == "array":
            function = self._universal_to_array(tree[1])
        else:
            print("Constructor '%s' is not supported" % str(tree), file=sys.stderr)

        return function

    def build_uni2nat(self):
        '''
        Build a read_* function
        '''
        out = [self.uni2nat_top]
        for m in self.manifolds.values():
            tree = self._parse_type(m.type)
            function = self.uni2nat(tree)
            # unpeal first level
            s = self.universal_to_natural.format(cast=function)
            # unpeal second - this allows {mid} to be expanded inside
            # the casting function
            s = s.format(mid=m.mid)
            out.append(s)
        return '\n'.join(out)

    def build_nat2uni(self):
        '''
        Build a show_* function
        '''
        out = [self.nat2uni_top]
        for m in self.manifolds.values():
            tree = self._parse_type(m.type)
            function = self.nat2uni(tree)
            s = self.natural_to_universal.format(cast=function)
            s = s.format(mid=m.mid)
            out.append(s)
        return '\n'.join(out)

    def _universal_to_primitive(self, typ):
        raise NotImplemented

    def _primitive_to_universal(self, typ):
        raise NotImplemented

    def _tuple_to_universal(self, typ):
        raise NotImplemented

    def _universal_to_tuple(self, typ):
        raise NotImplemented

    def _universal_to_array(self, typ):
        raise NotImplemented

    def _array_to_universal(self, typ):
        raise NotImplemented
