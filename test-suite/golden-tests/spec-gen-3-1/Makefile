all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	grep -c 'is the old spelling of the' build.err > obs.txt
	./nexus viaPy '{"x":1,"s":"a"}' >> obs.txt 2> obs.err
	./nexus viaCpp '{"x":1,"s":"a"}' >> obs.txt 2>> obs.err
	./nexus viaR '{"x":1,"s":"a"}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err *-build *build.tmp*
