all:
	rm -f *.err obs.txt
	rm -f /tmp/shared-registry-mpw-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testMultiPoolElementCount "/tmp/shared-registry-mpw-a.dat" >> obs.txt 2>> obs.err
	./nexus testMultiPoolDrainAll     "/tmp/shared-registry-mpw-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/shared-registry-mpw-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/shared-registry-mpw-*.dat
