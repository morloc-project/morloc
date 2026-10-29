all:
	rm -rf *.err obs.txt cache bump.py
	mkdir cache
	printf 'def bump(x):\n    return x + 100.0\n' > bump.py
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo 1 > obs.txt 2>> obs.err
	printf 'def bump(x):\n    return x + 200.0\n' > bump.py
	morloc make -o nexus main.loc 2>> build.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *-build *build.tmp* cache bump.py
