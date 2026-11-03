all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus allOf true true >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus allOf true false >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus anyOf false false >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus anyOf false true >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus pick true false >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus pick false true >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus pick false false >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus stopAnd true false >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus stopOr false true >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus pickThrow true true >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus pickThrow false false >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus pickThrow false true >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus classify true false >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus classify false true >> obs.txt 2>> obs.err || echo failed >> obs.txt
	./nexus classify false false >> obs.txt 2>> obs.err || echo failed >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *-build *build.tmp*
	rm -f *.err
