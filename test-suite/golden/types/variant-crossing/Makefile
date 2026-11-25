all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- areaPyCircle ---" >> obs.txt
	./nexus areaPy '{"Circle":[2.0]}' >> obs.txt 2>> obs.err
	echo "--- areaPyRect ---" >> obs.txt
	./nexus areaPy '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- areaPyDot ---" >> obs.txt
	./nexus areaPy '"Dot"' >> obs.txt 2>> obs.err
	echo "--- areaCpp ---" >> obs.txt
	./nexus areaCpp '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- areaR ---" >> obs.txt
	./nexus areaR '{"Rect":[3.0,4.0]}' >> obs.txt 2>> obs.err
	echo "--- areaRust ---" >> obs.txt
	./nexus areaRust '{"Circle":[2.0]}' >> obs.txt 2>> obs.err
	echo "--- roundTrip ---" >> obs.txt
	./nexus roundTrip '{"Rect":[1.0,2.0]}' >> obs.txt 2>> obs.err
	echo "--- shapes ---" >> obs.txt
	./nexus shapes '[{"Circle":[1.0]},{"Rect":[2.0,3.0]},"Dot"]' >> obs.txt 2>> obs.err
	echo "--- jsonHelpVariant ---" >> obs.txt
	if ./nexus --json-help 2>> obs.err | grep -q 'Circle'; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	echo "--- rejectsUnknownConstructor ---" >> obs.txt
	if ./nexus areaPy '{"Sphere":[1.0]}' >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
