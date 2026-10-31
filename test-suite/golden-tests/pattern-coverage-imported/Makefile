all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus area '{"Rect":[2.0,3.0]}' > obs.txt 2> obs.err
	morloc make -o partial partial.loc 2>> build.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo build_rejected >> obs.txt

clean:
	rm -f *.err nexus partial
	rm -rf *-build *build.tmp* __pycache__
