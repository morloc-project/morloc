all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- totalLeaf ---" >> obs.txt
	./nexus total '"Leaf"' >> obs.txt 2>> obs.err
	echo "--- totalTree ---" >> obs.txt
	./nexus total '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- sizeTree ---" >> obs.txt
	./nexus size '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- mirrorTree ---" >> obs.txt
	./nexus mirror '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- mirrorTwiceIsIdentity ---" >> obs.txt
	./nexus total '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- totalViaPy ---" >> obs.txt
	./nexus totalViaPy '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- totalViaCpp ---" >> obs.txt
	./nexus totalViaCpp '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- totalViaR ---" >> obs.txt
	./nexus totalViaR '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- totalViaRust ---" >> obs.txt
	./nexus totalViaRust '{"Node":[1.0,{"Node":[2.0,"Leaf","Leaf"]},{"Node":[3.0,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	echo "--- rejectsDanglingArity ---" >> obs.txt
	if ./nexus total '{"Node":[1.0,"Leaf"]}' >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
