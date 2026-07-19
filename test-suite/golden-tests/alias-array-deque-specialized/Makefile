all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testDequeI32Cp ---" > obs.txt
	./nexus testDequeI32Cp >> obs.txt 2>> obs.err
	echo "--- testDequeI32Pc ---" >> obs.txt
	./nexus testDequeI32Pc >> obs.txt 2>> obs.err
	echo "--- testDequeI64Cp ---" >> obs.txt
	./nexus testDequeI64Cp >> obs.txt 2>> obs.err
	echo "--- testDequeI64Pc ---" >> obs.txt
	./nexus testDequeI64Pc >> obs.txt 2>> obs.err
	echo "--- testDequeF32Cp ---" >> obs.txt
	./nexus testDequeF32Cp >> obs.txt 2>> obs.err
	echo "--- testDequeF32Pc ---" >> obs.txt
	./nexus testDequeF32Pc >> obs.txt 2>> obs.err
	echo "--- testDequeF64Cp ---" >> obs.txt
	./nexus testDequeF64Cp >> obs.txt 2>> obs.err
	echo "--- testDequeF64Pc ---" >> obs.txt
	./nexus testDequeF64Pc >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
