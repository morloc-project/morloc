all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus fact 0 > obs.txt 2> obs.err
	./nexus fact 1 >> obs.txt 2>> obs.err
	./nexus fact 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
