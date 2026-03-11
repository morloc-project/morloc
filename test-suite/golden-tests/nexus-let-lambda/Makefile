all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- letLambda ---" > obs.txt
	./nexus letLambda >> obs.txt 2> obs.err
	echo "--- letLambdaComplex ---" >> obs.txt
	./nexus letLambdaComplex >> obs.txt 2>> obs.err
	echo "--- letLambdaMultiUse ---" >> obs.txt
	./nexus letLambdaMultiUse >> obs.txt 2>> obs.err
	echo "--- letLambdaNested ---" >> obs.txt
	./nexus letLambdaNested >> obs.txt 2>> obs.err
	echo "--- letLambdaCapture ---" >> obs.txt
	./nexus letLambdaCapture >> obs.txt 2>> obs.err
	echo "--- letLambdaPartial ---" >> obs.txt
	./nexus letLambdaPartial 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log
