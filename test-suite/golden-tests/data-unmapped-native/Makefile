all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- enums ---" >> obs.txt
	./nexus enums >> obs.txt 2>> obs.err
	echo "--- variants ---" >> obs.txt
	./nexus variants >> obs.txt 2>> obs.err
	echo "--- roundTrip ---" >> obs.txt
	./nexus roundTrip '[{"Wrap":[1.0]},"Empty"]' >> obs.txt 2>> obs.err
	echo "--- enumsCpp ---" >> obs.txt
	./nexus enumsCpp >> obs.txt 2>> obs.err
	echo "--- variantsCpp ---" >> obs.txt
	./nexus variantsCpp >> obs.txt 2>> obs.err
	echo "--- eqSame ---" >> obs.txt
	./nexus eqSame >> obs.txt 2>> obs.err
	echo "--- eqDiff ---" >> obs.txt
	./nexus eqDiff >> obs.txt 2>> obs.err
	echo "--- eqSameCpp ---" >> obs.txt
	./nexus eqSameCpp >> obs.txt 2>> obs.err
	echo "--- crated ---" >> obs.txt
	./nexus crated >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err *.manifest
	rm -rf *-build *build.tmp*
