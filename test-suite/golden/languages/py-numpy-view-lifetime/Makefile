all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "view: $$(./nexus viewKind 1 2>> obs.err)" >> obs.txt
	echo "kept: $$(./nexus kept 30 2>> obs.err)" >> obs.txt
	./nexus makeTriples 300 triples.mlc 2>> obs.err
	echo "streamed: $$(./nexus streamed triples.mlc 2>> obs.err)" >> obs.txt

clean:
	rm -rf nexus pools *.err *.mlc __pycache__
	rm -rf *-build *build.tmp*
