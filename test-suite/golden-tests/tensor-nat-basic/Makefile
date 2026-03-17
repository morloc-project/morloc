all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testMakeVec ---" > obs.txt
	./nexus testMakeVec >> obs.txt 2>> obs.err
	echo "--- testMakeMat ---" >> obs.txt
	./nexus testMakeMat >> obs.txt 2>> obs.err
	echo "--- testId ---" >> obs.txt
	./nexus testId >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
