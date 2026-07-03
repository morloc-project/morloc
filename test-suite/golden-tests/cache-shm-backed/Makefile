# SHM-backed packet cache test across all three pool languages.
# --inline-size 0 forces every voidstar payload through shared memory:
# packets carry a relptr_t into SHM rather than inline bytes. Cache
# keys must hash schema-resolved VALUES (not relptr bits) and
# cache_store must materialize SHM-backed results to a self-contained
# format before writing. Two runs per language with the same input
# must serve the second from cache; cache/a/ ends up with exactly
# three packets (one per language).

INPUT := '[1.0,2.0,3.0,4.0,5.5]'

all:
	rm -f *.err obs.txt
	rm -rf cache
	mkdir cache
	morloc make --inline-size 0 -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2> obs.run.P1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2>> obs.run.P1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2> obs.run.R1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2>> obs.run.R1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2> obs.run.C1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2>> obs.run.C1.err
	@echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l)" >> obs.txt
	@echo "dats:      $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run.*.err > obs.err

clean:
	rm -rf nexus pools cache *.err
