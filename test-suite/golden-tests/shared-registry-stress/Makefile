all:
	rm -f *.err obs.txt
	rm -f /tmp/shared-registry-stress-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus stressIntCoalesce      "/tmp/shared-registry-stress-a.dat" >> obs.txt 2>> obs.err
	./nexus stressStrRoundtrip     "/tmp/shared-registry-stress-b.dat" >> obs.txt 2>> obs.err
	./nexus stressCrossPoolMixed   "/tmp/shared-registry-stress-c.dat" >> obs.txt 2>> obs.err
	# Lower buffer to 4 KiB for oversize-mixed; default 16 MiB
	# is too big to trigger element-level split with small ints.
	MORLOC_WRITE_BUFFER_BYTES=4096 ./nexus stressOversizeMixed "/tmp/shared-registry-stress-d.dat" >> obs.txt 2>> obs.err
	# 256 B buffer + 200-element Int write forces >16 sub-packets,
	# exercising shared sub-packet index growth past INITIAL_CAP.
	MORLOC_WRITE_BUFFER_BYTES=256 ./nexus stressManySubpackets "/tmp/shared-registry-stress-e.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/shared-registry-stress-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/shared-registry-stress-*.dat
