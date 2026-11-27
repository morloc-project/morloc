B = '{"pts":[{"x":1,"y":2},{"x":3,"y":4}],"best":{"x":3,"y":4}}'
all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip $(B) > obs.txt 2> obs.err
	./nexus roundTrip '{"pts":[],"best":null}' >> obs.txt 2>> obs.err
	./nexus kinds $(B) >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err *-build *build.tmp*
