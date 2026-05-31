all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- testDequeMonoidPy ---" > obs.txt
	./nexus testDequeMonoidPy >> obs.txt 2>> obs.err
	echo "--- testDequeMonoidCpp ---" >> obs.txt
	./nexus testDequeMonoidCpp >> obs.txt 2>> obs.err
	echo "--- testDequeMonoidR ---" >> obs.txt
	./nexus testDequeMonoidR >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
