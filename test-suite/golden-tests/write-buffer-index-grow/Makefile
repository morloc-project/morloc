all:
	rm -f *.err obs.txt
	rm -f /tmp/write-buffer-grow-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testIndexGrowTotalCount   "/tmp/write-buffer-grow-a.dat" >> obs.txt 2>> obs.err
	./nexus testIndexGrowFirstElement "/tmp/write-buffer-grow-b.dat" >> obs.txt 2>> obs.err
	./nexus testIndexGrowAtBoundary   "/tmp/write-buffer-grow-c.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/write-buffer-grow-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/write-buffer-grow-*.dat
