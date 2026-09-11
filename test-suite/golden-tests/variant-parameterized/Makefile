all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- fromPy ---" >> obs.txt
	./nexus fromPy 3 >> obs.txt 2>> obs.err
	echo "--- strFromPy ---" >> obs.txt
	./nexus strFromPy hi >> obs.txt 2>> obs.err
	echo "--- boxes ---" >> obs.txt
	./nexus boxes >> obs.txt 2>> obs.err
	echo "--- nested ---" >> obs.txt
	./nexus nested 5 >> obs.txt 2>> obs.err
	echo "--- pairFromPy ---" >> obs.txt
	./nexus pairFromPy a 2 >> obs.txt 2>> obs.err
	echo "--- intoPyFull ---" >> obs.txt
	./nexus intoPy '{"Full":[4]}' >> obs.txt 2>> obs.err
	echo "--- intoPyEmpty ---" >> obs.txt
	./nexus intoPy '"Empty"' >> obs.txt 2>> obs.err
	echo "--- cppGrow ---" >> obs.txt
	./nexus cppGrow '{"Full":[2]}' >> obs.txt 2>> obs.err
	echo "--- rustGrow ---" >> obs.txt
	./nexus rustGrow '{"Full":[2]}' >> obs.txt 2>> obs.err
	echo "--- roundTrip ---" >> obs.txt
	./nexus roundTrip '{"Full":[1]}' >> obs.txt 2>> obs.err
	echo "--- roundTripEmpty ---" >> obs.txt
	./nexus roundTrip '"Empty"' >> obs.txt 2>> obs.err
	echo "--- madeInRust ---" >> obs.txt
	./nexus madeInRust 7 >> obs.txt 2>> obs.err
	echo "--- nestedCrate ---" >> obs.txt
	./nexus nestedCrate 8 >> obs.txt 2>> obs.err
	echo "--- nexusNested ---" >> obs.txt
	./nexus nexusNested 9 >> obs.txt 2>> obs.err
	echo "--- nexusTree ---" >> obs.txt
	./nexus nexusTree 1 >> obs.txt 2>> obs.err
	echo "--- cppTree ---" >> obs.txt
	./nexus cppTree 1 >> obs.txt 2>> obs.err
	echo "--- rustTree ---" >> obs.txt
	./nexus rustTree 1 >> obs.txt 2>> obs.err
	echo "--- nonRegularRejected ---" >> obs.txt
	if morloc make -o nonregular nonregular.loc >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus nonregular pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
