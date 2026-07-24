all:
	: > obs.txt
	rm -f *.err f.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus -f packet produce 5 > f.dat 2>> obs.err
	# --- description from the imported alias reaches the arg's --help ---
	printf -- '--- readCmd --help carries the alias description + check.path ---\n' >> obs.txt
	./nexus readCmd --help 2>> obs.err | grep -E 'A readable filename|path to a readable file' >> obs.txt
	# --- stdin: true from the imported alias makes the arg optional (reads stdin) ---
	printf -- '--- readCmd STDIN (arg omitted) ---\n' >> obs.txt
	./nexus -f packet produce 5 | ./nexus readCmd >> obs.txt 2>> obs.err
	# --- check.path from the imported alias: a real file still works ---
	printf -- '--- readCmd FILE ---\n' >> obs.txt
	./nexus readCmd f.dat >> obs.txt 2>> obs.err
	# --- the terminal-action command (mosm cut shape) also gets an optional
	#     RFile arg from the imported alias: omitting it must NOT be a "required
	#     argument" error ---
	printf -- '--- streamCmd (render.buffer) with RFile omitted ---\n' >> obs.txt
	if ./nexus streamcmd '0:10' < /dev/null > /dev/null 2>> obs.err; then \
		echo "OK: terminal-action RFile arg is optional" >> obs.txt; \
	else \
		echo "BUG: terminal-action RFile arg required (alias docstring lost)" >> obs.txt; \
	fi

clean:
	rm -rf nexus pools __pycache__ *.err f.dat
