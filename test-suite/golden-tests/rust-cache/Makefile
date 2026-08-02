# Cache smoke test for a Rust pool. Two runs of the same input populate the
# cache once then hit it (one packet under cache/a); a distinct input adds a
# second packet.
all:
	rm -f *.err
	: > obs.txt
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR '[1,2,3.5]' >> obs.txt 2> obs.run.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR '[1,2,3.5]' >> obs.txt 2>> obs.run.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR '[10,20]'   >> obs.txt 2>> obs.run.err
	@echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run.err > obs.err

clean:
	rm -rf nexus pools cache *.err
