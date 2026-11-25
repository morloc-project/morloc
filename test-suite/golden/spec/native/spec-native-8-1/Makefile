all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus litU8 > obs.txt 2> obs.err
	./nexus cppU8 >> obs.txt 2>> obs.err
	./nexus litI64 >> obs.txt 2>> obs.err
	./nexus cppI64 >> obs.txt 2>> obs.err
	./nexus litI32 >> obs.txt 2>> obs.err
	./nexus cppI32 >> obs.txt 2>> obs.err
	./nexus litF32 >> obs.txt 2>> obs.err
	./nexus cppF32 >> obs.txt 2>> obs.err
	./nexus litBool >> obs.txt 2>> obs.err
	./nexus cppBool >> obs.txt 2>> obs.err
	./nexus litStr >> obs.txt 2>> obs.err
	./nexus cppStr >> obs.txt 2>> obs.err
	./nexus varU8 7 >> obs.txt 2>> obs.err
	./nexus poly >> obs.txt 2>> obs.err
	./nexus overU8 7 >> obs.txt 2>> obs.err || echo refused >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *err
	rm -rf *-build *build.tmp*
