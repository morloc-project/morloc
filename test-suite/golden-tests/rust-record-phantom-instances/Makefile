all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus run 2 > obs.txt 2> obs.err
	./nexus both 3 >> obs.txt 2>> obs.err
	morloc make -o reject reject.loc 2> reject.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo build_rejected >> obs.txt
	grep -c "error\[E" reject.err >> obs.txt || true

clean:
	rm -rf nexus reject pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
