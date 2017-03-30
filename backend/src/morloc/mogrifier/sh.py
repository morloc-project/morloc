from mogrifier.base_mogrifier import Mogrifier
import sys

universal_to_atom = {
    "Int"    : 'echo "$({x})"',
    "Num"    : 'echo "$({x})"',
    "String" : """{x} | sed 's/^"\|"$//g'""",
    "File"   : """{x} | sed 's/^"\|"$//g'""",
    "Bool"   : 'echo "$({x})"',
    "Text"   : 'cat <({x})',
    "Table"  : 'cat <({x})',
    "Void"   : 'cat <({x}) || echo "$({x})"',
    "*"      : 'cat <({x}) || echo "$({x})"'
}

atom_to_universal = {
    "Int"    : 'echo "$({x})"',
    "Num"    : 'echo "$({x})"',
    "String" : '''printf '"%s"' "$({x})"''',
    "File"   : '''printf '"%s"' "$({x})"''',
    "Bool"   : 'echo "$({x})"',
    "Text"   : 'cat <({x})',
    "Table"  : 'cat <({x})',
    "Void"   : 'cat <({x}) || echo "$({x})" > /dev/null',
    "*"      : 'cat <({x}) || echo "$({x})"'
}

uni2nat_top = ''
nat2uni_top = ''

universal_to_natural = '''
read_{{mid}} (){{{{
    # {comment}
    {cast}
}}}}
'''

natural_to_universal = '''
show_{{mid}} (){{{{
    # {comment}
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
        if typ == "Void":
            return "echo -n"
        else:
            return self.universal_to_atom[typ].format(x="cat $1")

    def _universal_to_tuple(self, typ):
        return "jq -r '@tsv' $1"

    def _wtf_to_universal(self, typ):
        return "# WTF, I can't do this"

    def _universal_to_wtf(self, typ):
        return "# WTF, I can't do this"

    def _universal_to_array(self, typ):
        if typ[0] == "atomic":
            s = """jq -r 'map(tostring) | join("\\n")' $1"""
        elif typ[0] == "array":
            s = """jq -r 'map(@tsv) | join("\\n")' $1"""
        else:
            s = "echo $1"
        return s

    def _primitive_to_universal(self, typ):
        s = self.atom_to_universal[typ].format(x="{mid} $@")
        return s

    def _tuple_to_universal(self, typ):
        return '''{mid} $@ | jq -R -s -c 'split("\\t")' '''

    def _array_to_universal(self, typ):
        if(typ[0] == "atomic"):
            return '''
    while read line
    do
        %s
        echo -n ','
    done < <({mid} $@) | sed 's/,$//; 1s/^/[/; $s/$/]/' | jq -c '.'
''' % atom_to_universal[typ[1]].format(x='echo $line')
        elif(typ[0] == "array"):
            cast = ""
            if (typ[1][0] == "atomic") and (typ[1][1] in ("String", "File", "Text")):
                cast = 'for(i=1; i<=NF; i++){{$i = "\\""$i"\\""}};'
            return '''
    awk '
        BEGIN{{FS="\\t"; OFS="\\t"; ORS=","}}
        {{%s print "[" $0 "]" }}
    ' <({mid} $@) | sed 's/\\t/,/g; s/\(.*\),/[\\1]/'
''' % cast
