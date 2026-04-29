all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testArrayInt32Cp ---" > obs.txt
	./nexus testArrayInt32Cp >> obs.txt 2>> obs.err
	echo "--- testArrayInt32Pc ---" >> obs.txt
	./nexus testArrayInt32Pc >> obs.txt 2>> obs.err
	echo "--- testArrayInt64Cp ---" >> obs.txt
	./nexus testArrayInt64Cp >> obs.txt 2>> obs.err
	echo "--- testArrayInt64Pc ---" >> obs.txt
	./nexus testArrayInt64Pc >> obs.txt 2>> obs.err
	echo "--- testArrayFloat32Cp ---" >> obs.txt
	./nexus testArrayFloat32Cp >> obs.txt 2>> obs.err
	echo "--- testArrayFloat32Pc ---" >> obs.txt
	./nexus testArrayFloat32Pc >> obs.txt 2>> obs.err
	echo "--- testArrayFloat64Cp ---" >> obs.txt
	./nexus testArrayFloat64Cp >> obs.txt 2>> obs.err
	echo "--- testArrayFloat64Pc ---" >> obs.txt
	./nexus testArrayFloat64Pc >> obs.txt 2>> obs.err
	echo "--- testDequeInt32Cp ---" >> obs.txt
	./nexus testDequeInt32Cp >> obs.txt 2>> obs.err
	echo "--- testDequeInt32Pc ---" >> obs.txt
	./nexus testDequeInt32Pc >> obs.txt 2>> obs.err
	echo "--- testDequeFloat32Cp ---" >> obs.txt
	./nexus testDequeFloat32Cp >> obs.txt 2>> obs.err
	echo "--- testDequeFloat32Pc ---" >> obs.txt
	./nexus testDequeFloat32Pc >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
