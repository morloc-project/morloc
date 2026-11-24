all:
	rm -f *.err obs.txt
	CXX="$${CXX:-g++} -fsanitize=address,undefined -fno-sanitize-recover=all" morloc make -o nexus main.loc 2> build.err
	ASAN_OPTIONS=detect_leaks=0 ./nexus rebuild 10 >> obs.txt 2>> obs.err
	ASAN_OPTIONS=detect_leaks=0 ./nexus shared 10 >> obs.txt 2>> obs.err
	ASAN_OPTIONS=detect_leaks=0 ./nexus twoHolders 10 >> obs.txt 2>> obs.err
	ASAN_OPTIONS=detect_leaks=0 ./nexus lentPart 10 >> obs.txt 2>> obs.err
	ASAN_OPTIONS=detect_leaks=0 ./nexus threeFields 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
