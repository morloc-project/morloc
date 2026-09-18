all:
	rm -f *.err ints.pkt
	: > obs.txt
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedFirst ints.pkt >> obs.txt 2>> obs.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedFirst ints.pkt >> obs.txt 2>> obs.err
	@echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l | tr -d ' ')" >> obs.txt
	rm -f ints.pkt

clean:
	rm -rf nexus pools __pycache__ cache *.err ints.pkt
	rm -rf *-build *build.tmp*
