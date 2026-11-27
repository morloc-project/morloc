all:
	rm -f *.err obs.txt
	morloc make -o reject main.loc 2> build.err && echo "UNEXPECTED_SUCCESS" > obs.txt || echo "build_rejected" > obs.txt
	grep -o -m1 "E0027" build.err >> obs.txt || true
	morloc make -o nexus accept.loc 2>> build.err
	./nexus second 3 2>> obs.err >> obs.txt

clean:
	rm -rf reject nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
