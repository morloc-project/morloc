all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- one-field ---" >> obs.txt
	./nexus area --circle 2.0 >> obs.txt 2>> obs.err
	echo "--- two-fields ---" >> obs.txt
	./nexus area --rect 2.0 3.5 >> obs.txt 2>> obs.err
	echo "--- nullary ---" >> obs.txt
	./nexus area --dot >> obs.txt 2>> obs.err
	echo "--- two-arms-refused ---" >> obs.txt
	if ./nexus area --dot --circle 1.0 >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- required-missing ---" >> obs.txt
	if ./nexus area >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- wrong-arity ---" >> obs.txt
	if ./nexus area --rect 2.0 >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- optional-absent ---" >> obs.txt
	./nexus describe >> obs.txt 2>> obs.err
	./nexus describe --rect 1.0 1.0 >> obs.txt 2>> obs.err
	echo "--- default ---" >> obs.txt
	./nexus count >> obs.txt 2>> obs.err
	./nexus count --rect 1.0 1.0 >> obs.txt 2>> obs.err
	echo "--- nested-constructor-value ---" >> obs.txt
	./nexus paint --solid red >> obs.txt 2>> obs.err
	./nexus paint --solid Blue >> obs.txt 2>> obs.err
	./nexus paint --bare >> obs.txt 2>> obs.err
	echo "--- string-field ---" >> obs.txt
	./nexus say --text hello >> obs.txt 2>> obs.err
	./nexus say --num 3 >> obs.txt 2>> obs.err
	echo "--- help ---" >> obs.txt
	./nexus area -h >> obs.txt 2>> obs.err
	echo "--- json-help ---" >> obs.txt
	./nexus --json-help | python3 -c 'import json,sys; m=json.load(sys.stdin); c=[x for x in m["commands"] if x["name"]=="count"][0]; a=c["arguments"][0]; print(json.dumps({k:a[k] for k in ("role","required","default")}, sort_keys=True)); print([ (r["long"], len(r["fields"])) for r in a["arms"]])' >> obs.txt 2>> obs.err
	echo "--- mcp ---" >> obs.txt
	./nexus --mcp-tools | python3 -c 'import json,sys; m=json.load(sys.stdin); t=[x for x in m["tools"] if x["name"]=="area"][0]; print(sorted(t["inputSchema"]["properties"].keys()), t["inputSchema"].get("required")); t=[x for x in m["tools"] if x["name"]=="say"][0]; print(sorted(t["inputSchema"]["properties"].keys()))' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
