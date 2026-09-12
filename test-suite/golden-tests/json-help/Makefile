all:
	rm -f *.err obs.txt
	morloc make -o main main.loc 2> build.err
	# --json-help: full morloc-native help as JSON. The morloc_version
	# value is normalized so the golden is stable across releases.
	printf -- '=== json-help ===\n' >> obs.txt
	./main --json-help 2>> obs.err \
	  | sed 's/"morloc_version": "[^"]*"/"morloc_version": "VERSION"/' >> obs.txt
	# --mcp-tools: MCP tools/list payload (no version field).
	printf -- '=== mcp-tools ===\n' >> obs.txt
	./main --mcp-tools 2>> obs.err >> obs.txt
	# An optional positional may be left off the command line. The CLI, the
	# json help and the MCP shape have to agree that it is not required, so
	# the call is exercised here beside the two views that describe it.
	printf -- '=== optional positional omitted ===\n' >> obs.txt
	./main maybeDouble >> obs.txt 2>> obs.err
	printf -- '=== optional positional supplied ===\n' >> obs.txt
	./main maybeDouble 21 >> obs.txt 2>> obs.err

clean:
	rm -rf main main.manifest pools __pycache__ obs.err build.err
	rm -rf *-build *build.tmp*
