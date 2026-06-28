all:
	rm -f *.err obs.txt /tmp/ifile-arr-*.pkt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testArrayCpp ---" > obs.txt
	./nexus testArrayCpp \
	    "/tmp/ifile-arr-cpp-a.pkt" \
	    "/tmp/ifile-arr-cpp-b.pkt" >> obs.txt 2>> obs.err
	echo "--- testArrayPy ---" >> obs.txt
	./nexus testArrayPy \
	    "/tmp/ifile-arr-py-a.pkt" \
	    "/tmp/ifile-arr-py-b.pkt" >> obs.txt 2>> obs.err
	echo "--- testArrayR ---" >> obs.txt
	./nexus testArrayR \
	    "/tmp/ifile-arr-r-a.pkt" \
	    "/tmp/ifile-arr-r-b.pkt" >> obs.txt 2>> obs.err
	echo "--- testArrayMixedDispatch ---" >> obs.txt
	./nexus testArrayMixedDispatch \
	    "/tmp/ifile-arr-mixed.pkt" >> obs.txt 2>> obs.err
	rm -f /tmp/ifile-arr-*.pkt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/ifile-arr-*.pkt
