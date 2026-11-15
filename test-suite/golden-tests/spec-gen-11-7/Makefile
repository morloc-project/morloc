all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip '{"pts":[{"x":1},{"x":2}]}' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
