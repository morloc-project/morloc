all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	./nexus py2c 3 4 > obs.txt 2> obs.err
	./nexus c2py 3 4 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
