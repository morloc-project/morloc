all:
	rm -rf *.err obs.txt cache bump.py bump.ref __pycache__
	mkdir cache
	printf 'def bump(x):\n    return x + 100.0\n' > bump.py
	cp -p bump.py bump.ref
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo 1 > obs.txt 2>> obs.err
	printf 'def bump(x):\n    return x + 200.0\n' > bump.py
	touch -r bump.ref bump.py
	morloc make -o nexus main.loc 2>> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *-build *build.tmp* cache bump.py bump.ref __pycache__
