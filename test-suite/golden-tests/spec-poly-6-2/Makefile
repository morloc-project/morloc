all:
	rm -f *.err obs.txt
	morloc make -o reject main.loc 2> build.err && echo "UNEXPECTED_SUCCESS" > obs.txt || echo "build_rejected" > obs.txt
	grep -c "morloc: \|CallStack" build.err >> obs.txt || true
	grep -o "^main.loc:[0-9:-]*" build.err >> obs.txt || true
	morloc make -o nexus accept.loc 2>> build.err
	./nexus f 1 2>> obs.err >> obs.txt

clean:
	rm -rf reject nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
