all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	python3 -c 'import json; m = json.load(open("nexus-build/manifest.json")); ls = [p["lang"] for p in m["pools"]]; [print(c["name"], " ".join(sorted(ls[i] for i in c["needed_pools"]))) for c in m["commands"] if not c["internal"]]' >> obs.txt 2> obs.err
	./nexus cppOnly 3 >> obs.txt 2>> obs.err
	./nexus pyOnly 3 >> obs.txt 2>> obs.err
	./nexus mixed 3 >> obs.txt 2>> obs.err
	./nexus nested 3 >> obs.txt 2>> obs.err
	./nexus viaC 1 >> obs.txt 2>> obs.err
	./nexus viaP 1 >> obs.txt 2>> obs.err
	./nexus countDownP 3 >> obs.txt 2>> obs.err
	./nexus callsExport 2 >> obs.txt 2>> obs.err
	./nexus bigC 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
