all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	morloc make -o swapped swapped.loc 2>> build.err
	morloc make -o modules modules.loc 2>> build.err
	morloc make -o cpp main-cpp.loc 2>> build.err
	./nexus pureFirstIO '[1,2,3]' > obs.txt 2> obs.err
	./nexus pureFirstPure '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus separateIO '[1,2,3]' >> obs.txt 2>> obs.err
	./nexus callbackIO 4 >> obs.txt 2>> obs.err
	./nexus callbackPure 4 >> obs.txt 2>> obs.err
	./swapped ioFirstIO '[1,2,3]' >> obs.txt 2>> obs.err
	./swapped ioFirstPure '[1,2,3]' >> obs.txt 2>> obs.err
	./modules viaB '[1,2,3]' >> obs.txt 2>> obs.err
	./modules viaA '[1,2,3]' >> obs.txt 2>> obs.err
	./cpp pureFirstIO '[1,2,3]' >> obs.txt 2>> obs.err
	./cpp pureFirstPure '[1,2,3]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus swapped modules cpp __pycache__ *.err
	rm -rf *-build *build.tmp*
