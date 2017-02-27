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

        if(tree[0] == "tuple"):
            if(all(s[0] == "atomic" for s in tree[1])):
                # primitive tuple, as TAB delimited list
            else:
                # general tuple, requires JSON
        elif(tree[0] == "atomic"):
            if tree[1] in literal:
                # atomic, as literal
            else:
                # atomic, as file
        elif(tree[0] == "array"):
            tree = tree[1]
            if(tree[0] == "array"):
                tree = tree[1]
                if tree[0] == "atomic":
                    # homogenous list of lists as TSV
                else:
                    # higher dimensional or complex matrix, requires JSON
            elif tree[0] == "tuple":
                tree = tree[1]
                if(all(s[0] == "atomic" for s in tree[1])):
                    # heterogenous proper table (headerless)
                else:
                    # complex table, requires JSON
            elif tree[0] == "atomic":
                # newline delimited list
            else:
                # error
        elif(tree[0] == "function"):
            # function
        else:
            # error

        return(function)

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
