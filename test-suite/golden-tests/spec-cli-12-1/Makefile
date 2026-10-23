all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus --json-help 2>> obs.err | python3 -c 'import json,sys; m=json.load(sys.stdin); print(json.dumps(m["program"]["epilogues"])); [print(c["name"], json.dumps(c["epilogues"])) for c in m["commands"]]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
