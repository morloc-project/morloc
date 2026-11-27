all:
	rm -f *.err obs.txt
	echo "--- cpp ---" > obs.txt
	morloc make -o main-cpp main-cpp.loc 2> build.err
	-./main-cpp ifLambdaMapped true 10 '[1,2]' >> obs.txt 2>> obs.err
	-./main-cpp ifLambdaMapped false 10 '[1,2]' >> obs.txt 2>> obs.err
	-./main-cpp ifLambdaDirect true 10 3 >> obs.txt 2>> obs.err
	-./main-cpp unusedRecord 5 >> obs.txt 2>> obs.err
	-./main-cpp unusedUnit 5 >> obs.txt 2>> obs.err
	-./main-cpp unusedForeign 5 >> obs.txt 2>> obs.err
	echo "--- rust ---" >> obs.txt
	morloc make -o main-rust main-rust.loc 2>> build.err
	-./main-rust ifLambdaMapped true 10 '[1,2]' >> obs.txt 2>> obs.err
	-./main-rust ifLambdaMapped false 10 '[1,2]' >> obs.txt 2>> obs.err
	-./main-rust ifLambdaDirect true 10 3 >> obs.txt 2>> obs.err
	-./main-rust unusedRecord 5 >> obs.txt 2>> obs.err
	-./main-rust unusedUnit 5 >> obs.txt 2>> obs.err
	-./main-rust unusedForeign 5 >> obs.txt 2>> obs.err

clean:
	rm -rf main-cpp main-rust __pycache__ *.err
	rm -rf *-build *build.tmp*
