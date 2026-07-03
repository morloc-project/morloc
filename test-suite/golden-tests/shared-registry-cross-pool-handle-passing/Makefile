all:
	rm -f *.err obs.txt
	rm -f /tmp/shared-registry-cphp-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus testRoundTrip "/tmp/shared-registry-cphp-a.dat" >> obs.txt 2>> obs.err
	rm -f /tmp/shared-registry-cphp-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/shared-registry-cphp-*.dat
