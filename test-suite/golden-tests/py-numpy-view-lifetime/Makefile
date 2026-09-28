RUN = MORLOC_PY_POOL=thread

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "view: $$($(RUN) ./nexus viewKind 1 2>> obs.err)" >> obs.txt
	echo "kept: $$($(RUN) ./nexus kept 30 2>> obs.err)" >> obs.txt
	./nexus makeTriples 300 triples.mlc 2>> obs.err
	echo "streamed: $$($(RUN) ./nexus streamed triples.mlc 2>> obs.err)" >> obs.txt
	echo "streamed-fork: $$(./nexus streamed triples.mlc 2>> obs.err)" >> obs.txt

clean:
	rm -rf nexus pools *.err *.mlc __pycache__
	rm -rf *-build *build.tmp*
