all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus rustLocal 200 >> obs.txt 2>> obs.err
	./nexus rustForeign 200 >> obs.txt 2>> obs.err
	./nexus cppLocal 200 >> obs.txt 2>> obs.err
	./nexus cppForeign 200 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
