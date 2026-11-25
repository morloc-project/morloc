all:
	rm -f *.err obs.txt
	morloc make -o nexus-py main-py.loc 2> build-py.err
	./nexus-py f null >> obs.txt 2>> obs.err
	./nexus-py f 5 >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust f null >> obs.txt 2>> obs.err
	./nexus-rust f 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-* *-build *build.tmp* *.err __pycache__
