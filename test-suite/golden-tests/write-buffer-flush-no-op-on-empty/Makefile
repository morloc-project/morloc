all:
	rm -f *.err obs.txt
	rm -f /tmp/write-buffer-flush-noop-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testEmptyFlushIsNoop   "/tmp/write-buffer-flush-noop-a.dat" >> obs.txt 2>> obs.err
	./nexus testEmptyFlushThenEof  "/tmp/write-buffer-flush-noop-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/write-buffer-flush-noop-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/write-buffer-flush-noop-*.dat
