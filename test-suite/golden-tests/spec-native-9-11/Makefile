all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus literal 1 >> obs.txt 2>> obs.err
	./nexus received '{"x":1,"y":"a"}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *-build *build.tmp*
