all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/write-buffer-oversize-*.dat
	morloc make -o nexus main.loc 2> build.err
	# Lower buffer to 4 KiB so 8-byte Int elements split at 510 entries.
	# Default buffer is 16 MiB.
	MORLOC_WRITE_BUFFER_BYTES=4096 ./nexus testFirstSubpacket "/tmp/write-buffer-oversize-a.dat" >> obs.txt 2>> obs.err
	MORLOC_WRITE_BUFFER_BYTES=4096 ./nexus testTotalCount     "/tmp/write-buffer-oversize-b.dat" >> obs.txt 2>> obs.err
	MORLOC_WRITE_BUFFER_BYTES=4096 ./nexus testSplitSize      "/tmp/write-buffer-oversize-c.dat" >> obs.txt 2>> obs.err
	MORLOC_WRITE_BUFFER_BYTES=4096 ./nexus testEof            "/tmp/write-buffer-oversize-d.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/write-buffer-oversize-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/write-buffer-oversize-*.dat
