all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- pyThunkInCpp ---" > obs.txt
	./nexus pyThunkInCpp >> obs.txt 2>> obs.err
	echo "--- multiPyThunkInCpp ---" >> obs.txt
	./nexus multiPyThunkInCpp >> obs.txt 2>> obs.err
	echo "--- mixedThunkInCpp ---" >> obs.txt
	./nexus mixedThunkInCpp >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
