all:
	rm -f *.err
	: > obs.txt
	# Decoy files whose names collide with JSON literals. Their
	# contents are nonsense Ints so that any test which DOES open
	# them produces a clearly wrong number. With the new classifier
	# the JSON-shape preempt fires first and these files are never
	# read.
	echo 999 > 42
	echo 888 > null
	morloc make -o main main.loc > /dev/null 2> build.err
	# many on [Int]: inline / file / mixed
	./main sumInts 1 2 3 2>> obs.err >> obs.txt
	./main sumInts a.json b.json c.json 2>> obs.err >> obs.txt
	./main sumInts 1 b.json 3 2>> obs.err >> obs.txt
	# pure-morloc many round-trip
	./main passList 1 2 3 2>> obs.err >> obs.txt
	# many + literal on [Str]: tokens are literal strings, NOT files
	./main countLits foo.txt bar.txt baz.txt 2>> obs.err >> obs.txt
	./main countLits a.json b.json c.json 2>> obs.err >> obs.txt
	# option-form many: provided / omitted (default [])
	./main concatLists --xs 1 2 3 2>> obs.err >> obs.txt
	./main concatLists 2>> obs.err >> obs.txt
	# scalar literal on Str: literal CLI value / file-named token / default
	./main greet -y hello 2>> obs.err >> obs.txt
	./main greet -y a.json 2>> obs.err >> obs.txt
	./main greet 2>> obs.err >> obs.txt
	# typed default: not provided / provided
	./main defaults 2>> obs.err >> obs.txt
	./main defaults -i 7 2>> obs.err >> obs.txt
	# JSON-shape preempts file_exists: token `null` -> JSON null,
	# token `42` -> JSON 42 (not the decoy file contents of 999).
	./main nullKeyword null 2>> obs.err >> obs.txt
	./main nullKeyword 42 2>> obs.err >> obs.txt
	# recursive-record default validation: omitted (default tree) /
	# user-supplied tree.
	./main recParam 2>> obs.err >> obs.txt
	./main recParam --tree '{"val":99,"children":[]}' 2>> obs.err >> obs.txt
	# Decoy cleanup.
	rm -f 42 null

clean:
	rm -rf main main.manifest pools __pycache__ obs.txt obs.err build.err 42 null
