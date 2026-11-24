all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pairFst '[2.5,"s"]' > obs.txt 2> obs.err
	./nexus listHead '[[1.5,2.5],[3.0]]' >> obs.txt 2>> obs.err
	./nexus fieldHead '{"x":[4.5]}' >> obs.txt 2>> obs.err
	./nexus listHead -h 2>&1 | grep 'type:' >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
