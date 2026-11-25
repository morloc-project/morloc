all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- nexusEq ---" >> obs.txt
	./nexus nexusEq Green >> obs.txt 2>> obs.err
	./nexus nexusEq Red >> obs.txt 2>> obs.err
	echo "--- nexusLt ---" >> obs.txt
	./nexus nexusLt Red >> obs.txt 2>> obs.err
	./nexus nexusLt Blue >> obs.txt 2>> obs.err
	echo "--- eqR ---" >> obs.txt
	./nexus eqR Green >> obs.txt 2>> obs.err
	./nexus eqR Red >> obs.txt 2>> obs.err
	echo "--- ltR ---" >> obs.txt
	./nexus ltR Red >> obs.txt 2>> obs.err
	./nexus ltR Blue >> obs.txt 2>> obs.err
	echo "--- geR ---" >> obs.txt
	./nexus geR Green >> obs.txt 2>> obs.err
	./nexus geR Red >> obs.txt 2>> obs.err
	echo "--- eqPy ---" >> obs.txt
	./nexus eqPy Green >> obs.txt 2>> obs.err
	./nexus eqPy Red >> obs.txt 2>> obs.err
	echo "--- ltPy ---" >> obs.txt
	./nexus ltPy Red >> obs.txt 2>> obs.err
	./nexus ltPy Blue >> obs.txt 2>> obs.err
	echo "--- eqCpp ---" >> obs.txt
	./nexus eqCpp Green >> obs.txt 2>> obs.err
	./nexus eqCpp Red >> obs.txt 2>> obs.err
	echo "--- ltCpp ---" >> obs.txt
	./nexus ltCpp Red >> obs.txt 2>> obs.err
	./nexus ltCpp Blue >> obs.txt 2>> obs.err
	echo "--- eqRust ---" >> obs.txt
	./nexus eqRust Green >> obs.txt 2>> obs.err
	./nexus eqRust Red >> obs.txt 2>> obs.err
	echo "--- ltRust ---" >> obs.txt
	./nexus ltRust Red >> obs.txt 2>> obs.err
	./nexus ltRust Blue >> obs.txt 2>> obs.err
	echo "--- rFormAgrees ---" >> obs.txt
	./nexus formWireR Blue >> obs.txt 2>> obs.err
	./nexus formLitR >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
