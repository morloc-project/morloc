all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus rustToPy 3 >> obs.txt 2>> obs.err
	./nexus pyToRust 3 >> obs.txt 2>> obs.err
	./nexus rustOnly 4 >> obs.txt 2>> obs.err
	./nexus recordsInList 4 >> obs.txt 2>> obs.err
	./nexus rowParamData 7 >> obs.txt 2>> obs.err
	./nexus cppNative 5 >> obs.txt 2>> obs.err
	./nexus cppHeld 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
