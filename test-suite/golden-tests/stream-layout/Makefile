all:
	rm -f *.err
	: > obs.txt
	rm -f /tmp/stream-layout-*.dat
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- offsets ---\n' >> obs.txt
	./nexus offsets "/tmp/stream-layout-a.dat" >> obs.txt 2>> obs.err
	printf -- '--- counts ---\n' >> obs.txt
	./nexus counts "/tmp/stream-layout-b.dat" >> obs.txt 2>> obs.err
	printf -- '--- numChunks ---\n' >> obs.txt
	./nexus numChunks "/tmp/stream-layout-c.dat" >> obs.txt 2>> obs.err
	printf -- '--- totalElems ---\n' >> obs.txt
	./nexus totalElems "/tmp/stream-layout-e.dat" >> obs.txt 2>> obs.err
	printf -- '--- dataOffsets ---\n' >> obs.txt
	./nexus dataOffsets "/tmp/stream-layout-f.dat" >> obs.txt 2>> obs.err
	printf -- '--- dataCounts ---\n' >> obs.txt
	./nexus dataCounts "/tmp/stream-layout-g.dat" >> obs.txt 2>> obs.err
	printf -- '--- reject @streamLayout on OStream ---\n' >> obs.txt
	if morloc make -o bad_ostream bad_ostream.loc 2>> obs.err; then \
		echo "UNEXPECTED: OStream accepted" >> obs.txt; \
	else \
		echo "OK: OStream rejected" >> obs.txt; \
	fi
	printf -- '--- reject @streamLayout on non-list IFile ---\n' >> obs.txt
	if morloc make -o bad_nonlist bad_nonlist.loc 2>> obs.err; then \
		echo "UNEXPECTED: non-list IFile accepted" >> obs.txt; \
	else \
		echo "OK: non-list IFile rejected" >> obs.txt; \
	fi
	rm -f /tmp/stream-layout-*.dat

clean:
	rm -rf nexus bad_ostream bad_nonlist pools __pycache__ *.err
	rm -f /tmp/stream-layout-*.dat
