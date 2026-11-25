RUN = MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo

all:
	rm -rf *.err obs.txt cache bump.R bump.ref __pycache__
	mkdir cache
	printf 'bump <- function(x) x + 100\n' > bump.R
	cp bump.R bump.ref
	morloc make -o nexus main.loc 2> build.err
	$(RUN) 1 > obs.txt 2>> obs.err
	printf 'bump <- function(x) x + 200\n' > bump.R
	$(RUN) 2 >> obs.txt 2> edited.err; echo "edited: exit $$?" >> obs.txt
	grep -c 'bump.R has changed since the program was built; rebuild' edited.err >> obs.txt || true
	cp bump.ref bump.R
	$(RUN) 2 >> obs.txt 2>> obs.err
	rm bump.R
	$(RUN) 3 >> obs.txt 2> missing.err; echo "missing: exit $$?" >> obs.txt
	grep -c 'cannot read sourced file .*bump.R' missing.err >> obs.txt || true
	cp bump.ref bump.R

clean:
	rm -rf nexus pools *-build *build.tmp* cache bump.R bump.ref __pycache__
