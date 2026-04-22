all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testArrayMonoidPy ---" > obs.txt
	./nexus testArrayMonoidPy >> obs.txt 2>> obs.err
	echo "--- testArrayMonoidCpp ---" >> obs.txt
	./nexus testArrayMonoidCpp >> obs.txt 2>> obs.err
	echo "--- testArrayMonoidR ---" >> obs.txt
	./nexus testArrayMonoidR >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
