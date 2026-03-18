all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testAdd ---" > obs.txt
	./nexus testAdd >> obs.txt 2>> obs.err
	echo "--- testTranspose ---" >> obs.txt
	./nexus testTranspose >> obs.txt 2>> obs.err
	echo "--- testMatmul ---" >> obs.txt
	./nexus testMatmul >> obs.txt 2>> obs.err
	echo "--- testTrace ---" >> obs.txt
	./nexus testTrace >> obs.txt 2>> obs.err
	echo "--- testDiag ---" >> obs.txt
	./nexus testDiag >> obs.txt 2>> obs.err
	echo "--- testVstack ---" >> obs.txt
	./nexus testVstack >> obs.txt 2>> obs.err
	echo "--- testHstack ---" >> obs.txt
	./nexus testHstack >> obs.txt 2>> obs.err
	echo "--- testOuter ---" >> obs.txt
	./nexus testOuter >> obs.txt 2>> obs.err
	echo "--- testDot ---" >> obs.txt
	./nexus testDot >> obs.txt 2>> obs.err
	echo "--- testFlatten ---" >> obs.txt
	./nexus testFlatten >> obs.txt 2>> obs.err
	echo "--- testKron ---" >> obs.txt
	./nexus testKron >> obs.txt 2>> obs.err
	echo "--- testSlice ---" >> obs.txt
	./nexus testSlice >> obs.txt 2>> obs.err
	echo "--- testTake ---" >> obs.txt
	./nexus testTake >> obs.txt 2>> obs.err
	echo "--- testSliceAdd ---" >> obs.txt
	./nexus testSliceAdd >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
