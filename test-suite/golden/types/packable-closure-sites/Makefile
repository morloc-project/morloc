all:
	rm -rf *.err obs.txt cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	./nexus cppTwoCaps 5 >> obs.txt 2>> obs.err || echo "cppTwoCaps failed" >> obs.txt
	./nexus cppLambda 5 >> obs.txt 2>> obs.err || echo "cppLambda failed" >> obs.txt
	./nexus cppOneCap 5 >> obs.txt 2>> obs.err || echo "cppOneCap failed" >> obs.txt
	./nexus cppArg 5 >> obs.txt 2>> obs.err || echo "cppArg failed" >> obs.txt
	./nexus cppResult 5 >> obs.txt 2>> obs.err || echo "cppResult failed" >> obs.txt
	./nexus rsResult 5 >> obs.txt 2>> obs.err || echo "rsResult failed" >> obs.txt
	./nexus rsCaps 5 >> obs.txt 2>> obs.err || echo "rsCaps failed" >> obs.txt
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cppCached 5 >> obs.txt 2>> obs.err || echo "cppCached failed" >> obs.txt
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cppCached 5 >> obs.txt 2>> obs.err || echo "cppCached failed" >> obs.txt
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus rsCached 5 >> obs.txt 2>> obs.err || echo "rsCached failed" >> obs.txt
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus rsCached 5 >> obs.txt 2>> obs.err || echo "rsCached failed" >> obs.txt
	./nexus pyCaps 5 >> obs.txt 2>> obs.err || echo "pyCaps failed" >> obs.txt
	./nexus pyArg 5 >> obs.txt 2>> obs.err || echo "pyArg failed" >> obs.txt
	./nexus pyResult 5 >> obs.txt 2>> obs.err || echo "pyResult failed" >> obs.txt
	./nexus rCaps 5 >> obs.txt 2>> obs.err || echo "rCaps failed" >> obs.txt
	./nexus rArg 5 >> obs.txt 2>> obs.err || echo "rArg failed" >> obs.txt
	./nexus rResult 5 >> obs.txt 2>> obs.err || echo "rResult failed" >> obs.txt

clean:
	rm -rf nexus pools __pycache__ cache
	rm -rf *-build *build.tmp*
	rm -f *.err
