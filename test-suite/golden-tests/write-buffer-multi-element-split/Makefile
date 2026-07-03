all:
	rm -f *.err obs.txt
	rm -f /tmp/write-buffer-split-*.dat
	morloc make -o nexus main.loc 2> build.err
	MORLOC_WRITE_BUFFER_BYTES=4096 ./nexus testTotalCount   "/tmp/write-buffer-split-a.dat" >> obs.txt 2>> obs.err
	MORLOC_WRITE_BUFFER_BYTES=4096 ./nexus testFirstElement "/tmp/write-buffer-split-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/write-buffer-split-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/write-buffer-split-*.dat
