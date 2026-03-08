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
	echo "--- diamondStr ---" >> obs.txt
	./nexus diamondStr >> obs.txt 2>> obs.err
	echo "--- diamondList ---" >> obs.txt
	./nexus diamondList >> obs.txt 2>> obs.err
	echo "--- letSharedAcrossLangs ---" >> obs.txt
	./nexus letSharedAcrossLangs >> obs.txt 2>> obs.err
	echo "--- deepDoChain ---" >> obs.txt
	./nexus deepDoChain >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
