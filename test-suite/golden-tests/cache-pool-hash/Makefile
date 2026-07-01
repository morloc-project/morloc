# Source-edit cache invalidation across all three pool languages.
# Build twice with the same labeled function but a different body
# (the constant in `fold (+) 0.0` -> `fold (+) 100.0`) and the same
# input on both runs. The per-pool source fingerprint must shift,
# the cache key must change, and the second run must recompute --
# yielding 106.5 instead of the stale 6.5.

INPUT := '[1,2,3.5]'

all:
	rm -f *.err obs.txt
	rm -rf cache
	mkdir cache
	@# If a previous run crashed between the sed-edit and the
	@# restore-from-backup step, main.loc is left in the edited
	@# state. Recover by moving the backup back BEFORE snapshotting.
	@if [ -f main.orig.loc ]; then mv -f main.orig.loc main.loc; fi
	cp main.loc main.orig.loc
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2> obs.run.P1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2> obs.run.R1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2> obs.run.C1.err
	sed -i 's/fold (+) 0\.0/fold (+) 100.0/' main.loc
	morloc make -o nexus main.loc 2> build2.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2> obs.run.P2.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2> obs.run.R2.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2> obs.run.C2.err
	mv main.orig.loc main.loc
	@echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l)" >> obs.txt
	@echo "dats:      $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run.*.err > obs.err

clean:
	rm -rf nexus pools cache *.err main.orig.loc build2.err
