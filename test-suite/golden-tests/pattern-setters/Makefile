all:
	rm -f *.err obs.txt
	morloc make -o nexus-py main-py.loc 2> build.err
	morloc make -o nexus-r main-r.loc 2> build.err
	morloc make -o nexus-cpp main-cpp.loc 2> build.err
	./nexus-py test > obs.txt 2> obs.err
	./nexus-r test 2>> obs.err  >> obs.txt
	./nexus-cpp test 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
