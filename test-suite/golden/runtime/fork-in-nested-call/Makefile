all:
	rm -f obs.txt *.err calls.log
	morloc make -o nexus main.loc 2> build.err
	./nexus mk 4 > obs.txt 2>> obs.err
	cat calls.log >> obs.txt
	rm -f calls.log
	morloc make -o nexus-r main-r.loc 2>> build.err
	./nexus-r outer 4 >> obs.txt 2>> obs.err
	cat calls.log >> obs.txt
	rm -f calls.log
	./nexus-r caught 4 >> obs.txt 2>> obs.err
	touch calls.log
	cat calls.log >> obs.txt

clean:
	rm -rf nexus nexus-r pools *.err calls.log
	rm -rf *-build *build.tmp*
