# Body-exception test across all three pool languages. Per language,
# the labeled function's foreign body raises; the cache must stay
# empty (failed runs are not memoized) and a second call with the
# same input must fail the same way -- not get served silently from
# a stale or partial entry.

INPUT := 1.0

all:
	rm -f *.err obs.txt
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	@for lang in P R C; do \
	  MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$$lang $(INPUT) > obs.run.$$lang.1.out 2>&1 || true; \
	  echo "run1_$${lang}_raised: $$(grep -c 'INTENTIONAL_TEST_FAILURE' obs.run.$$lang.1.out)" >> obs.txt; \
	  MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$$lang $(INPUT) > obs.run.$$lang.2.out 2>&1 || true; \
	  echo "run2_$${lang}_raised: $$(grep -c 'INTENTIONAL_TEST_FAILURE' obs.run.$$lang.2.out)" >> obs.txt; \
	done
	echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l)" >> obs.txt
	echo "dats:      $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run.*.out > obs.err
	rm -f obs.run.*.out

clean:
	rm -rf nexus pools cache *.err obs.run.*.out
