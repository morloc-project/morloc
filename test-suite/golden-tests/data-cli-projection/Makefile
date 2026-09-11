all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- rank-cases ---" >> obs.txt
	./nexus rank High >> obs.txt 2>> obs.err
	./nexus rank high >> obs.txt 2>> obs.err
	./nexus rank HIGH >> obs.txt 2>> obs.err
	echo "--- rank-unknown ---" >> obs.txt
	if ./nexus rank Urgent >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- pick-default ---" >> obs.txt
	./nexus pick >> obs.txt 2>> obs.err
	./nexus pick -p low >> obs.txt 2>> obs.err
	echo "--- shapes-many ---" >> obs.txt
	./nexus shapes -p low -p HIGH -p Medium >> obs.txt 2>> obs.err
	echo "--- describe-payload ---" >> obs.txt
	./nexus describe '{"Rect":[2.0,3.0]}' >> obs.txt 2>> obs.err
	./nexus describe dot >> obs.txt 2>> obs.err
	echo "--- help-rank ---" >> obs.txt
	./nexus rank -h >> obs.txt 2>> obs.err
	echo "--- help-pick ---" >> obs.txt
	./nexus pick -h >> obs.txt 2>> obs.err
	echo "--- help-describe ---" >> obs.txt
	./nexus describe -h >> obs.txt 2>> obs.err
	echo "--- json-help ---" >> obs.txt
	./nexus --json-help | python3 -c 'import json,sys; m=json.load(sys.stdin); print(json.dumps([t for t in m["types"] if t["kind"]=="data"], sort_keys=True))' >> obs.txt 2>> obs.err
	echo "--- mcp-tools ---" >> obs.txt
	./nexus --mcp-tools | python3 -c 'import json,sys; m=json.load(sys.stdin); t=[x for x in m["tools"] if x["name"]=="pick"][0]; print(json.dumps(t["inputSchema"]["properties"], sort_keys=True))' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
