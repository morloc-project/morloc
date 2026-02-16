all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- cppForce ---" > obs.txt
	./nexus cppForce >> obs.txt 2>> obs.err
	echo "--- pyForce ---" >> obs.txt
	./nexus pyForce >> obs.txt 2>> obs.err
	echo "--- rForce ---" >> obs.txt
	./nexus rForce >> obs.txt 2>> obs.err
	echo "--- crossCppPy ---" >> obs.txt
	./nexus crossCppPy >> obs.txt 2>> obs.err
	echo "--- crossPyR ---" >> obs.txt
	./nexus crossPyR >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
