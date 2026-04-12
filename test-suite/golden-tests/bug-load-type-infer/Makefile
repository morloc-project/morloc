all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTrip 42 '"/tmp/bug-load-type-infer.bin"' > obs.txt 2> obs.err
	rm -f /tmp/bug-load-type-infer.bin

clean:
	rm -rf nexus pools __pycache__ *err
	rm -f /tmp/bug-load-type-infer.bin
