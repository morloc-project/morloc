all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus-cpp main-cpp.loc 2> build.err
	./nexus-cpp both '{"Box":["a",[1,2,3]]}' >> obs.txt 2>> obs.err
	./nexus-cpp both '"Empty"' >> obs.txt 2>> obs.err
	./nexus-cpp reuse '{"Box":["a",[1,2,3]]}' >> obs.txt 2>> obs.err
	./nexus-cpp chain '{"Box":["a",[1,2,3]]}' >> obs.txt 2>> obs.err
	./nexus-cpp pull '{"Box":["a",[1,2,3]]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp __pycache__ *.err
	rm -rf *-build *build.tmp*
