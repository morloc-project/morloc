all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo "[1.0,2.0,3.0]" > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
