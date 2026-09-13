all:
	: > obs.txt
	rm -rf pools
	morloc make -o nexus main.loc 2> build.err
	./nexus mmSum >> obs.txt 2> obs.err
	./nexus vScaledSum 2.0 '[1,2,3,4]' >> obs.txt 2>> obs.err
	if [ -d nexus-build/pools/cpp/cpp-guests/futhark ] && [ ! -d pools ]; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	cp nexus-build/pools/cpp/pool.cpp first.cpp
	morloc make -o nexus main.loc 2>> build.err
	if cmp -s first.cpp nexus-build/pools/cpp/pool.cpp; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -rf nexus pools build.err obs.err __pycache__ first.cpp
	rm -rf *-build *build.tmp*
