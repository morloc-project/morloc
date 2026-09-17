all:
	rm -f *.err
	: > obs.txt
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	./nexus chain 2 >> obs.txt 2>> obs.err
	./nexus chain 100000 2>> obs.err | ./nexus count - >> obs.txt 2>> obs.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedCount 100000 >> obs.txt 2>> obs.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedCount 100000 >> obs.txt 2>> obs.err
	@echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l | tr -d ' ')" >> obs.txt
	./nexus showRead 100000 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ cache *.err
	rm -rf *-build *build.tmp*
