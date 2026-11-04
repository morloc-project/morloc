all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus mk 4 > obs.txt 2> obs.err
	./nexus len3 '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus len3 '[1,2]' >> obs.txt 2>&1 || echo "exit $$?" >> obs.txt
	./nexus len4 '[1,2,3,4]' >> obs.txt 2>> obs.err
	./nexus len4 '[1,2,3]' >> obs.txt 2>&1 || echo "exit $$?" >> obs.txt
	./nexus lenE '[1,2]' >> obs.txt 2>&1 || echo "exit $$?" >> obs.txt
	./nexus sum3 '[1,2]' >> obs.txt 2>&1 || echo "exit $$?" >> obs.txt
	./nexus --json-help | python3 -c 'import json,sys; print([a["type"]["morloc"] for c in json.load(sys.stdin)["commands"] if c["name"] in ("sum3", "lenE", "len4") for a in c["arguments"]])' >> obs.txt

clean:
	rm -rf nexus pools *err __pycache__
	rm -rf *-build *build.tmp*
