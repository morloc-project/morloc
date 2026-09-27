all:
	: > obs.txt
	rm -f *.err s.pkt
	morloc make -o nexus main.loc 2> build.err
	./nexus -f json twoSites >> obs.txt 2>> obs.err
	./nexus -f jsonl twoSites >> obs.txt 2>> obs.err
	./nexus -f packet -o s.pkt twoSites 2>> obs.err
	./nexus total s.pkt >> obs.txt 2>> obs.err
	./nexus seqRet 7 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err s.pkt
	rm -rf *-build *build.tmp*
