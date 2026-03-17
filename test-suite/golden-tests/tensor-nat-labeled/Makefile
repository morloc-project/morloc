all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- predictDigit ---" > obs.txt
	./nexus predictDigit >> obs.txt 2>> obs.err
	echo "--- testConv ---" >> obs.txt
	./nexus testConv >> obs.txt 2>> obs.err
	echo "--- testRelu ---" >> obs.txt
	./nexus testRelu >> obs.txt 2>> obs.err
	echo "--- testFlatten ---" >> obs.txt
	./nexus testFlatten >> obs.txt 2>> obs.err
	echo "--- testDense ---" >> obs.txt
	./nexus testDense >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
