all:
	rm -f *.err obs.txt *.mpk
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp trip t.mpk 3 >> obs.txt 2>> obs.err
	morloc make -o nexus-py main-py.loc 2> build-py.err && echo "UNEXPECTED_SUCCESS" >> obs.txt || grep -c "level by level" build-py.err >> obs.txt
	morloc make -o nexus-user main-user.loc 2> build-user.err && echo "UNEXPECTED_SUCCESS" >> obs.txt || grep -c "level by level" build-user.err >> obs.txt
	morloc make -o nexus-root main-root.loc 2> build-root.err
	./nexus-root tripR '{"v":1,"kids":[{"v":2,"kids":[]}]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-py nexus-user nexus-root pools __pycache__ *err *.mpk *-build *build.tmp*
