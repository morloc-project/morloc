all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- manyCallsSmall ---" > obs.txt
	./nexus manyCallsSmall >> obs.txt 2> obs.err
	echo "--- manyCallsLarge ---" >> obs.txt
	./nexus manyCallsLarge >> obs.txt 2>> obs.err
	echo "--- chainCppPy ---" >> obs.txt
	./nexus chainCppPy >> obs.txt 2>> obs.err
	echo "--- chainPyCpp ---" >> obs.txt
	./nexus chainPyCpp >> obs.txt 2>> obs.err
	echo "--- deepDoChain ---" >> obs.txt
	./nexus deepDoChain >> obs.txt 2>> obs.err
	echo "--- nestedLetCross ---" >> obs.txt
	./nexus nestedLetCross >> obs.txt 2>> obs.err
	echo "--- listAppendCross ---" >> obs.txt
	./nexus listAppendCross >> obs.txt 2>> obs.err
	echo "--- resultForwardChain ---" >> obs.txt
	./nexus resultForwardChain >> obs.txt 2>> obs.err
	echo "--- bigStrRoundTrip ---" >> obs.txt
	./nexus bigStrRoundTrip >> obs.txt 2>> obs.err
	echo "--- bigVecRoundTrip ---" >> obs.txt
	./nexus bigVecRoundTrip >> obs.txt 2>> obs.err
	echo "--- vecOfStrsRoundTrip ---" >> obs.txt
	./nexus vecOfStrsRoundTrip >> obs.txt 2>> obs.err
	echo "--- nestedVecRoundTrip ---" >> obs.txt
	./nexus nestedVecRoundTrip >> obs.txt 2>> obs.err
	echo "--- bigStrChain ---" >> obs.txt
	./nexus bigStrChain >> obs.txt 2>> obs.err
	echo "--- manyBigStrings ---" >> obs.txt
	./nexus manyBigStrings >> obs.txt 2>> obs.err
	echo "--- manyNestedVecs ---" >> obs.txt
	./nexus manyNestedVecs >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
