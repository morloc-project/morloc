all:
	rm -f obs.txt *.err calls.log
	morloc make -o nexus main.loc 2> build.err
	./nexus mk 5 > obs.txt 2>> obs.err
	wc -l < calls.log >> obs.txt

clean:
	rm -rf nexus pools *.err calls.log
	rm -rf *-build *build.tmp*
