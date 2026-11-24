all:
	rm -f obs.txt *.err calls.log
	morloc make -o nexus main.loc 2> build.err
	./nexus mk 5 > obs.txt 2>> obs.err
	./nexus mkP 1 >> obs.txt 2>> obs.err
	./nexus mkPR 1 >> obs.txt 2>> obs.err
	LC_ALL=C sort calls.log | uniq -c | awk '{print $$1, $$2}' >> obs.txt

clean:
	rm -rf nexus pools *.err calls.log
	rm -rf *-build *build.tmp*
