all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- letMinimal ---" > obs.txt
	./nexus letMinimal >> obs.txt 2> obs.err
	echo "--- letMinimalReverse ---" >> obs.txt
	./nexus letMinimalReverse >> obs.txt 2>> obs.err
	echo "--- letTripleChain ---" >> obs.txt
	./nexus letTripleChain >> obs.txt 2>> obs.err
	echo "--- letDiamond ---" >> obs.txt
	./nexus letDiamond >> obs.txt 2>> obs.err
	echo "--- letTripleUse ---" >> obs.txt
	./nexus letTripleUse >> obs.txt 2>> obs.err
	echo "--- letDeepChain ---" >> obs.txt
	./nexus letDeepChain >> obs.txt 2>> obs.err
	echo "--- letInDoBlock ---" >> obs.txt
	./nexus letInDoBlock >> obs.txt 2>> obs.err
	echo "--- letListFanOut ---" >> obs.txt
	./nexus letListFanOut >> obs.txt 2>> obs.err
	echo "--- letListDeepChain ---" >> obs.txt
	./nexus letListDeepChain >> obs.txt 2>> obs.err
	echo "--- letStrIdChain ---" >> obs.txt
	./nexus letStrIdChain >> obs.txt 2>> obs.err
	echo "--- letMixedTypes ---" >> obs.txt
	./nexus letMixedTypes >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
