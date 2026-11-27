all:
	: > obs.txt
	rm -f *.err s.pkt
	morloc make -o nexus main.loc 2> build.err
	morloc typecheck main.loc 2>> obs.err | grep -E '^(pick|plain) ::' >> obs.txt
	./nexus pick true 2 >> obs.txt 2>> obs.err
	./nexus pick false 2 >> obs.txt 2>> obs.err
	./nexus pick true 3 --all >> obs.txt 2>> obs.err
	./nexus -f packet -o s.pkt pick true 3 2>> obs.err
	./nexus total s.pkt >> obs.txt 2>> obs.err
	./nexus plain -h 2>> obs.err | grep '^Return' >> obs.txt
	./nexus pick -h 2>> obs.err | sed -n '/^Return:/,/^$$/p' >> obs.txt
	./nexus --json-help | python3 -c 'import json,sys; m=json.load(sys.stdin); [print(c["name"], c["return"]["streaming"], c["return"]["type"]["morloc"]) for c in m["commands"] if c["name"] in ("pick","plain")]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__ *.err s.pkt
	rm -rf *-build *build.tmp*
