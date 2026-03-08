all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- effectChain ---" > obs.txt
	./nexus effectChain >> obs.txt 2> obs.err
	echo "--- effectFanOut ---" >> obs.txt
	./nexus effectFanOut >> obs.txt 2>> obs.err
	echo "--- letForwardedTwice ---" >> obs.txt
	./nexus letForwardedTwice >> obs.txt 2>> obs.err
	echo "--- nestedLetCross ---" >> obs.txt
	./nexus nestedLetCross >> obs.txt 2>> obs.err
	echo "--- listAppendCross ---" >> obs.txt
	./nexus listAppendCross >> obs.txt 2>> obs.err
	echo "--- resultForwardChain ---" >> obs.txt
	./nexus resultForwardChain >> obs.txt 2>> obs.err
	echo "--- effectInLetRhs ---" >> obs.txt
	./nexus effectInLetRhs >> obs.txt 2>> obs.err
	echo "--- tripleForward ---" >> obs.txt
	./nexus tripleForward >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
