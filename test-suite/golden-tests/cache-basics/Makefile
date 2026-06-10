# cache:true smoke test across all three pool languages.
# Each export pins the labeled sum into one pool. Two runs per export
# should populate the cache once and hit it the second time. The
# label is shared across pools; per-pool keys differ (pool_hash +
# midx differ per pool) so cache/a/ ends up with one packet per
# language = 3 total.

INPUT := '[1,2,3.5]'

all:
	rm -f *.err obs.txt
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2> obs.runP.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2>> obs.runP.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2> obs.runR.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2>> obs.runR.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2> obs.runC.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2>> obs.runC.err
	@echo "a_packets: $$(ls cache/a/*.packet 2>/dev/null | wc -l)" >> obs.txt
	@echo "dats:      $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run*.err > obs.err

clean:
	rm -rf nexus pools cache *.err
