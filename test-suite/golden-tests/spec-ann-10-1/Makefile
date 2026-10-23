export MORLOC_CACHE_BASE := $(CURDIR)/cache
RUN = sh -c './$(EXE) "$$@" >> obs.txt 2> run.err; cat run.err >> obs.txt' --

all:
	rm -rf *.err obs.txt cache
	morloc make -o nexus main.loc 2> build.err
	$(MAKE) --no-print-directory runs EXE=nexus
	rm -rf cache
	echo '=== --debug ===' >> obs.txt
	morloc make --debug -o debug main.loc 2>> build.err
	$(MAKE) --no-print-directory runs EXE=debug

runs:
	echo '--- named ---' >> obs.txt; $(RUN) named 1
	echo '--- whereBound ---' >> obs.txt; $(RUN) whereBound 4
	echo '--- argBound ---' >> obs.txt; $(RUN) argBound 5
	echo '--- cachedNamed ---' >> obs.txt; $(RUN) cachedNamed 1; $(RUN) cachedNamed 1
	echo '--- cachedCall ---' >> obs.txt; $(RUN) cachedCall 4; $(RUN) cachedCall 5; $(RUN) cachedCall 4
	echo '--- cachedCpp ---' >> obs.txt; $(RUN) cachedCpp 6; $(RUN) cachedCpp 6
	echo '--- namedCpp ---' >> obs.txt; $(RUN) namedCpp 0
	echo '--- picked ---' >> obs.txt; $(RUN) picked 4; $(RUN) picked 4; $(RUN) picked -1; $(RUN) picked -1
	echo '--- mixed ---' >> obs.txt; $(RUN) mixed 5; $(RUN) mixed 5
	echo '--- cachedRs ---' >> obs.txt; $(RUN) cachedRs 6; $(RUN) cachedRs 6

clean:
	rm -rf nexus debug __pycache__ cache *.err
	rm -rf *-build *build.tmp*
