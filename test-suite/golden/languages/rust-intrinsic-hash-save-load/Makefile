all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/rust-hsl-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus hashScalar 42 >> obs.txt 2>> obs.err
	./nexus hashRecord 3 4 >> obs.txt 2>> obs.err
	./nexus saveLoadRecord "/tmp/rust-hsl-rec.dat" >> obs.txt 2>> obs.err
	./nexus saveLoadList   "/tmp/rust-hsl-list.dat" >> obs.txt 2>> obs.err
	./nexus saveJsonLoad   "/tmp/rust-hsl-json.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/rust-hsl-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/rust-hsl-*.dat
	rm -rf *-build *build.tmp*
