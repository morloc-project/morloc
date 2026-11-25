all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip ann > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
