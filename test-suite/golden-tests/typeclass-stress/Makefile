all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testPy ---" > obs.txt
	./nexus testPy >> obs.txt 2>> obs.err
	echo "--- testCpp ---" >> obs.txt
	./nexus testCpp >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
