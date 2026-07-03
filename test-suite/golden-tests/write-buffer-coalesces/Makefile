all:
	rm -f *.err obs.txt
	rm -f /tmp/write-buffer-coalesces-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testCoalesceAll      "/tmp/write-buffer-coalesces-a.dat" >> obs.txt 2>> obs.err
	./nexus testOneSubpacketOnly "/tmp/write-buffer-coalesces-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/write-buffer-coalesces-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/write-buffer-coalesces-*.dat
