all:
	rm -f *.err obs.txt
	morloc make -o main main.loc 2> build.err
	# The glossary must define every name the help prints and must not
	# publish a wire form for a constructor the program never packs.
	printf -- '=== json-help ===\n' >> obs.txt
	./main --json-help 2>> obs.err \
	  | sed 's/"morloc_version": "[^"]*"/"morloc_version": "VERSION"/' >> obs.txt
	printf -- '=== touch -h ===\n' >> obs.txt
	./main touch -h >> obs.txt 2>> obs.err

clean:
	rm -rf main main.manifest pools __pycache__ obs.err build.err
	rm -rf *-build *build.tmp*
