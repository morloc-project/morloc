all:
	rm -f *.err obs.txt *.dat
	morloc make -o nexus-py main-py.loc 2> build-py.err
	./nexus-py loadEnums e.dat >> obs.txt 2>> obs.err
	./nexus-py loadRecords r.dat >> obs.txt 2>> obs.err
	./nexus-py loadNested n.dat >> obs.txt 2>> obs.err
	./nexus-py single s.dat >> obs.txt 2>> obs.err
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp single c.dat >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust single u.dat >> obs.txt 2>> obs.err
	morloc make -o nexus-r main-r.loc 2> build-r.err
	./nexus-r single q.dat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-* *-build *build.tmp* *.err *.dat __pycache__
