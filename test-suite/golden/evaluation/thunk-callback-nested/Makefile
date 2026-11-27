all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- nestedLambdas ---" >> obs.txt
	./nexus nestedLambdas >> obs.txt 2>> obs.err
	echo "--- nestedBare ---" >> obs.txt
	./nexus nestedBare >> obs.txt 2>> obs.err
	echo "--- nestedMulti ---" >> obs.txt
	./nexus nestedMulti >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
