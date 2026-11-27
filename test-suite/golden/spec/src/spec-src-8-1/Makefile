RUN = MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo

all:
	rm -rf *.err obs.txt cache bump.py bump.ref __pycache__
	mkdir cache
	printf 'def bump(x):\n    return x + 100.0\n' > bump.py
	cp bump.py bump.ref
	morloc make -o nexus main.loc 2> build.err
	$(RUN) 1 > obs.txt 2>> obs.err
	printf 'def bump(x):\n    return x + 200.0\n' > bump.py
	$(RUN) 2 >> obs.txt 2> edited.err; echo "edited: exit $$?" >> obs.txt
	grep -c 'bump.py has changed since the program was built; rebuild' edited.err >> obs.txt || true
	cp bump.ref bump.py
	$(RUN) 2 >> obs.txt 2>> obs.err
	rm bump.py
	$(RUN) 3 >> obs.txt 2> missing.err; echo "missing: exit $$?" >> obs.txt
	grep -c 'cannot read sourced file .*bump.py' missing.err >> obs.txt || true
	cp bump.ref bump.py

clean:
	rm -rf nexus pools *-build *build.tmp* cache bump.py bump.ref __pycache__
