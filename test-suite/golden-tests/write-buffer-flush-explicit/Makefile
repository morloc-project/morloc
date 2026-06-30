all:
	rm -f *.err obs.txt
	rm -f /tmp/write-buffer-flush-explicit-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testFlushFirst  "/tmp/write-buffer-flush-explicit-a.dat" >> obs.txt 2>> obs.err
	./nexus testFlushSecond "/tmp/write-buffer-flush-explicit-b.dat" >> obs.txt 2>> obs.err
	./nexus testFlushThird  "/tmp/write-buffer-flush-explicit-c.dat" >> obs.txt 2>> obs.err
	./nexus testFlushEof    "/tmp/write-buffer-flush-explicit-d.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/write-buffer-flush-explicit-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/write-buffer-flush-explicit-*.dat
