all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus cppMono 1     >  obs.txt 2>  obs.err
	./nexus cppPolyRet 1  >> obs.txt 2>> obs.err
	./nexus cppTryRet 1   >> obs.txt 2>> obs.err
	./nexus cppPolyArg 1  >> obs.txt 2>> obs.err
	./nexus rustPolyRet 1 >> obs.txt 2>> obs.err
	./nexus cppRecord 3 >> obs.txt 2>> obs.err
	./nexus cppRecordEmpty 3 >> obs.txt 2>> obs.err
	./nexus cppRecordCallback 3 >> obs.txt 2>> obs.err
	./nexus cppTupleAlias 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
