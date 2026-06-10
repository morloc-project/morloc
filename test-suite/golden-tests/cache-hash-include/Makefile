# hash-include cache invalidation across all three pool languages.
# Edit extra.txt between builds; every pool's source fingerprint
# shifts (the hash-include contents fold into every pool's hash), so
# each language gets a new cache key the second time around.
#
# After two builds * three languages: six packets total (3 per
# build). The two builds return the same value so only one dat file
# is created (content-addressed dedup).
#
# The test writes the starting state of extra.txt explicitly so a
# previous failed run that left extra.txt in the edited state
# doesn't poison this run.

INPUT := '[1,2,3.5]'

all:
	rm -f *.err obs.txt
	rm -rf cache
	mkdir cache
	@printf 'version_one\n' > extra.txt
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2> obs.run.P1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2> obs.run.R1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2> obs.run.C1.err
	@printf 'version_two\n' > extra.txt
	morloc make -o nexus main.loc 2> build2.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2> obs.run.P2.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2> obs.run.R2.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2> obs.run.C2.err
	@printf 'version_one\n' > extra.txt
	@echo "a_packets: $$(ls cache/a/*.packet 2>/dev/null | wc -l)" >> obs.txt
	@echo "dats:      $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run.*.err > obs.err

clean:
	rm -rf nexus pools cache *.err build2.err
