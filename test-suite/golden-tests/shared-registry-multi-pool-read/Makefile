all:
	rm -f *.err obs.txt
	rm -f /tmp/shared-registry-mpr-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testCrossPoolReaders    "/tmp/shared-registry-mpr-a.dat" >> obs.txt 2>> obs.err
	./nexus testCrossPoolReadersEof "/tmp/shared-registry-mpr-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/shared-registry-mpr-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/shared-registry-mpr-*.dat
