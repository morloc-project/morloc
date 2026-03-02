all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus riskyCall > obs.txt 2> obs.err
	./nexus safeCall >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
