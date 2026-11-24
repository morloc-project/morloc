all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus f '{"x":1,"ys":[2]}' > obs.txt 2> obs.err
	./nexus g '[{"Sq":[2]}]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err *-build *build.tmp*
