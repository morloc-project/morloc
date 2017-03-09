from mogrifier.base_mogrifier import Mogrifier
import sys

universal_to_atom = {
    "Int"    : 'echo "$({x})"',
    "Num"    : 'echo "$({x})"',
    "String" : """{x} | sed 's/^"\|"$//g'""",
    "File"   : """{x} | sed 's/^"\|"$//g'""",
    "Bool"   : 'test $({x}) == "true" && echo 1 || echo 0 ',
    "Text"   : 'cat <({x})',
    "void"   : 'echo -n',
    "*"      : 'cat <({x}) || echo "$({x})"'
}

atom_to_universal = {
    "Int"    : 'echo "$({x})"',
    "Num"    : 'echo "$({x})"',
    "String" : '''printf '"%s"' "$({x})"''',
    "File"   : '''printf '"%s"' "$({x})"''',
    "Bool"   : """test $({x}) -eq 1 && echo 'true' || echo 'false'""",
    "Text"   : 'cat <({x})',
    "void"   : 'echo -n',
    "*"      : 'cat <({x}) || echo "$({x})"'
}

uni2nat_top = ''
nat2uni_top = ''

universal_to_natural = '''
read_{{mid}} (){{{{
    {cast}
}}}}
'''

natural_to_universal = '''
show_{{mid}} (){{{{
    {cast}
}}}}
'''

class ShMogrifier(Mogrifier):
    def __init__(self, manifolds):

        super().__init__(manifolds) 

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
        return self.universal_to_atom[typ].format(x="cat $1")

    def _universal_to_tuple(self, typ):
        return "jq -r '@tsv' $1"

    def _universal_to_array(self, typ):
        if(typ[0] == "atomic"):
            if(typ[1] == "Bool"):
                return """jq -r 'map(tostring) | join("\\n")' $1 | sed 's/true/1/; s/false/0/'"""
            else:
                return """jq -r 'map(tostring) | join("\\n")' $1"""
        elif(typ[0] == "array"):
            return """jq -r 'map(@tsv) | join("\\n")' $1"""
        else:
            return "echo $1"

    def _primitive_to_universal(self, typ):
        s = self.atom_to_universal[typ].format(x="{mid}")
        return s

    def _tuple_to_universal(self, typ):
        return '''
    echo -n '['
    sed 's/.*/"&"/' <($x) | tr '\n' ',' | sed 's/,$//'
    echo ']'
'''

    def _array_to_universal(self, typ):
        if(typ[0] == "atomic"):
            return '''
    echo -n '['
    while read line
    do
        %s
        echo -n ','
    done < <({mid}) | sed 's/,$//'
    echo ']'
''' % atom_to_universal[typ[1]].format(x='echo $line')
        elif(typ[0] == "array"):
            return '''
    echo -n '['
    while read line
    do
        printf '[%s],' $(tr '\\t' ',' <<< $line)
    done < <({mid}) | sed 's/.$//'
    echo ']'
'''
