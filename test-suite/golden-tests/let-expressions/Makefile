all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testCppOnce ---" > obs.txt
	./nexus testCppOnce 5 >> obs.txt 2>&1
	echo "--- testPyOnce ---" >> obs.txt
	./nexus testPyOnce 5 >> obs.txt 2>&1
	echo "--- testROnce ---" >> obs.txt
	./nexus testROnce 5 >> obs.txt 2>&1
	echo "--- testCppSeq ---" >> obs.txt
	./nexus testCppSeq 5 >> obs.txt 2>&1
	echo "--- testPySeq ---" >> obs.txt
	./nexus testPySeq 5 >> obs.txt 2>&1
	echo "--- testRSeq ---" >> obs.txt
	./nexus testRSeq 5 >> obs.txt 2>&1
	echo "--- testShadow ---" >> obs.txt
	./nexus testShadow 5 >> obs.txt 2>&1

clean:
	rm -rf nexus pools log __pycache__
