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

clean:
	rm -rf main main.manifest pools __pycache__ obs.err build.err
