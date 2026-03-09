all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testAliasPy ---" > obs.txt
	./nexus testAliasPy >> obs.txt 2>> obs.err
	echo "--- testAliasCpp ---" >> obs.txt
	./nexus testAliasCpp >> obs.txt 2>> obs.err
	echo "--- testInt32Cpp ---" >> obs.txt
	./nexus testInt32Cpp >> obs.txt 2>> obs.err
	echo "--- testDeepChainPy ---" >> obs.txt
	./nexus testDeepChainPy >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
