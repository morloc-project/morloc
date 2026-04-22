all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testConcatPy ---" > obs.txt
	./nexus testConcatPy >> obs.txt 2>> obs.err
	echo "--- testConcatCpp ---" >> obs.txt
	./nexus testConcatCpp >> obs.txt 2>> obs.err
	echo "--- testConcatR ---" >> obs.txt
	./nexus testConcatR >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
