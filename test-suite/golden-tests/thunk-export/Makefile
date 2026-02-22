all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- rollOnce ---" > obs.txt
	./nexus rollOnce >> obs.txt 2>> obs.err
	echo "--- rollArg ---" >> obs.txt
	./nexus rollArg 3 >> obs.txt 2>> obs.err
	echo "--- rollPy ---" >> obs.txt
	./nexus rollPy >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
