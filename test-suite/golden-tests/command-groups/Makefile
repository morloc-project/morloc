all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus math double 5 > obs.txt 2> obs.err
	./nexus math triple 3 >> obs.txt 2>> obs.err
	./nexus text greet '"hello"' >> obs.txt 2>> obs.err
	./nexus version >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools
