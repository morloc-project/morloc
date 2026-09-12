all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- describeRust ---" >> obs.txt
	./nexus describeRust '{"Rename":["a","b"]}' >> obs.txt 2>> obs.err
	echo "--- describeCpp ---" >> obs.txt
	./nexus describeCpp '{"Chmod":["f",7]}' >> obs.txt 2>> obs.err
	echo "--- describeNoop ---" >> obs.txt
	./nexus describeRust '"Noop"' >> obs.txt 2>> obs.err
	echo "--- stepRust ---" >> obs.txt
	./nexus stepRust '{"n":3,"action":{"Mkdir":["d"]}}' >> obs.txt 2>> obs.err
	echo "--- stepCpp ---" >> obs.txt
	./nexus stepCpp '{"n":4,"action":{"Rename":["x","y"]}}' >> obs.txt 2>> obs.err
	echo "--- planRust ---" >> obs.txt
	./nexus planRust '{"label":"p","first":{"Mkdir":["d"]},"level":"Fail"}' >> obs.txt 2>> obs.err
	echo "--- planCpp ---" >> obs.txt
	./nexus planCpp '{"label":"q","first":"Noop","level":"Info"}' >> obs.txt 2>> obs.err
	echo "--- madeRust ---" >> obs.txt
	./nexus madeRust built >> obs.txt 2>> obs.err
	echo "--- crossed ---" >> obs.txt
	./nexus crossed both >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err *.manifest
	rm -rf *-build *build.tmp*
