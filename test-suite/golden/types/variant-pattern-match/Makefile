all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- areaCircle ---" >> obs.txt
	./nexus area '{"Circle":[2.0]}' >> obs.txt 2>> obs.err
	echo "--- areaRect ---" >> obs.txt
	./nexus area '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- areaDot ---" >> obs.txt
	./nexus area '"Dot"' >> obs.txt 2>> obs.err
	echo "--- describeDegenerate ---" >> obs.txt
	./nexus describe '{"Circle":[0.0]}' >> obs.txt 2>> obs.err
	echo "--- describeRound ---" >> obs.txt
	./nexus describe '{"Circle":[2.0]}' >> obs.txt 2>> obs.err
	echo "--- describeBox ---" >> obs.txt
	./nexus describe '{"Rect":[1.0,2.0]}' >> obs.txt 2>> obs.err
	echo "--- describeDot ---" >> obs.txt
	./nexus describe '"Dot"' >> obs.txt 2>> obs.err
	echo "--- unwrapNested ---" >> obs.txt
	./nexus unwrapArea '{"Wrap":[{"Circle":[2.0]}]}' >> obs.txt 2>> obs.err
	echo "--- unwrapGeneral ---" >> obs.txt
	./nexus unwrapArea '{"Wrap":[{"Rect":[3.0,4.0]}]}' >> obs.txt 2>> obs.err
	echo "--- unwrapEmpty ---" >> obs.txt
	./nexus unwrapArea '"Empty"' >> obs.txt 2>> obs.err
	echo "--- areaOfPy ---" >> obs.txt
	./nexus areaOfPy '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- areaOfCpp ---" >> obs.txt
	./nexus areaOfCpp '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- areaOfR ---" >> obs.txt
	./nexus areaOfR '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- areaOfRust ---" >> obs.txt
	./nexus areaOfRust '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- rejectsUnknownConstructor ---" >> obs.txt
	if ./nexus area '{"Sphere":[1.0]}' >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	echo "--- rejectsWrongArity ---" >> obs.txt
	if ./nexus area '{"Circle":[1.0,2.0]}' >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
