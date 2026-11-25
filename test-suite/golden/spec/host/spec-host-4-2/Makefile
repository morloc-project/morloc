all:
	rm -f *.err obs.txt
	morloc make -o cpp-alias cpp-alias.loc 2> build.err
	./cpp-alias total abc > obs.txt 2> obs.err
	morloc make -o cpp-paren cpp-paren.loc 2>> build.err
	./cpp-paren total abc >> obs.txt 2>> obs.err
	morloc make -o cpp-flat cpp-flat.loc 2>> build.err
	./cpp-flat total abc >> obs.txt 2>> obs.err
	morloc make -o cpp-reader cpp-reader.loc 2>> build.err
	./cpp-reader total abc >> obs.txt 2>> obs.err
	morloc make -o rust-alias rust-alias.loc 2>> build.err
	./rust-alias total abc >> obs.txt 2>> obs.err
	morloc make -o rust-paren rust-paren.loc 2>> build.err
	./rust-paren total abc >> obs.txt 2>> obs.err
	morloc make -o rust-flat rust-flat.loc 2>> build.err
	./rust-flat total abc >> obs.txt 2>> obs.err
	morloc make -o rust-reader rust-reader.loc 2>> build.err
	./rust-reader total abc >> obs.txt 2>> obs.err

clean:
	rm -rf cpp-alias cpp-paren cpp-flat cpp-reader rust-alias rust-paren rust-flat rust-reader pools *err
	rm -rf *-build *build.tmp*
