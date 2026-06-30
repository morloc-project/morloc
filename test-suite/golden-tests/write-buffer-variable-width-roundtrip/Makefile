all:
	rm -f *.err obs.txt
	rm -f /tmp/write-buffer-strs-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testStringsRoundtrip "/tmp/write-buffer-strs-a.dat" >> obs.txt 2>> obs.err
	./nexus testStringsCount     "/tmp/write-buffer-strs-b.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/write-buffer-strs-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/write-buffer-strs-*.dat
