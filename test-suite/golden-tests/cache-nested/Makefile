# Nested labeled cache across all three pool languages. Per language,
# foo applies `a@map b@sum` over a list of sublists. After one run
# per language we expect one 'a' entry (the whole map call) and two
# 'b' entries (one per sublist input). After three languages: 3
# a-packets, 6 b-packets.

INPUT := '[[1,2,3.5],[4,5]]'

all:
	rm -f *.err obs.txt
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2> obs.run.P1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooP $(INPUT) >> obs.txt 2>> obs.run.P1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2> obs.run.R1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooR $(INPUT) >> obs.txt 2>> obs.run.R1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2> obs.run.C1.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus fooC $(INPUT) >> obs.txt 2>> obs.run.C1.err
	@echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l)" >> obs.txt
	@echo "b_packets: $$(ls cache/b/*.dat 2>/dev/null | wc -l)" >> obs.txt
	@echo "dats:      $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run.*.err > obs.err

clean:
	rm -rf nexus pools cache *.err
