all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- bareCallback ---" >> obs.txt
	./nexus bareCallback >> obs.txt 2>> obs.err
	echo "--- lambdaCallback ---" >> obs.txt
	./nexus lambdaCallback >> obs.txt 2>> obs.err
	echo "--- bareMultiEffect ---" >> obs.txt
	./nexus bareMultiEffect >> obs.txt 2>> obs.err
	echo "--- lambdaMultiEffect ---" >> obs.txt
	./nexus lambdaMultiEffect >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
