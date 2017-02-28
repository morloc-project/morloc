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

def typeIdent(): return _('[A-Za-z0-9_]+')
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

    def build_function(self, tree, direction):

        function = None

        def is_simple(s):
            return s[0] == "atomic" and s[1] in primitive

        if tree[0] == "atomic":
            if tree[1] in primitive:
                # atomic, as literal
            else:
                # atomic, as file
        elif tree[0] == "tuple":
            if all(is_simple(s) for s in tree[1]):
                # primitive tuple, as TAB delimited list
            else:
                # general tuple, requires JSON
        elif tree[0] == "array":
            if tree[1][0] == "atomic":
                if tree[1][1] in primitive: 
                    # newline delimited list
                else:
                    # complex list, JSON
            elif tree[1][0] == "tuple":
                if all(is_simple(s) for s in tree[1][1]):
                    # [(a,b,c,...)] - heterogenous proper table (headerless)
                else:
                    # complex table, requires JSON
            elif tree[1][0] == "array":
                if is_simple(tree[1][1]):
                    # [[a]] - homogenous list of lists as TSV
                else:
                    # [[[...a...]]] - higher dimensional or complex matrix, requires JSON
            else:
                # ERROR: unsupported constructor
        else:
            # ERROR: unsupported constructor

        return function

    def build_uni2nat(self):
        out = [self.uni2nat_top]
        for m in self.manifolds.values():
            tree = self._parse_type(m.type)

            function = build_function(tree, "uni2nat")

            function_name = "read_" + m.mid
            s = self.universal_to_natural.format(
                name=function_name,
                function=function
            )

            out.append(s)
        return '\n'.join(out)

    def build_nat2uni(self):
        out = [self.nat2uni_top]
        for m in self.manifolds.values():
            function_name = "show_" + m.mid

            type_tree = self._parse_type(m.type)

            function = build_function(tree, "nat2uni")

            s = self.natural_to_universal.format(
                name=function_name,
                caster=caster
            )

            out.append(s)
        return '\n'.join(out)

    def _parse_type(self, otype):
        parse_tree = self.parser.parse(otype)
        type_tree = visit_parse_tree(parse_tree, typeTypeVisitor())
        return(type_tree)

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
