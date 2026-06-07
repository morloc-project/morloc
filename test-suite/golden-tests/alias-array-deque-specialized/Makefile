all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testDequeInt32Cp ---" > obs.txt
	./nexus testDequeInt32Cp >> obs.txt 2>> obs.err
	echo "--- testDequeInt32Pc ---" >> obs.txt
	./nexus testDequeInt32Pc >> obs.txt 2>> obs.err
	echo "--- testDequeInt64Cp ---" >> obs.txt
	./nexus testDequeInt64Cp >> obs.txt 2>> obs.err
	echo "--- testDequeInt64Pc ---" >> obs.txt
	./nexus testDequeInt64Pc >> obs.txt 2>> obs.err
	echo "--- testDequeFloat32Cp ---" >> obs.txt
	./nexus testDequeFloat32Cp >> obs.txt 2>> obs.err
	echo "--- testDequeFloat32Pc ---" >> obs.txt
	./nexus testDequeFloat32Pc >> obs.txt 2>> obs.err
	echo "--- testDequeFloat64Cp ---" >> obs.txt
	./nexus testDequeFloat64Cp >> obs.txt 2>> obs.err
	echo "--- testDequeFloat64Pc ---" >> obs.txt
	./nexus testDequeFloat64Pc >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
