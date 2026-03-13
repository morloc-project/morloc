all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- foo ---" > obs.txt
	./nexus foo >> obs.txt 2>> obs.err
	echo "--- bar ---" >> obs.txt
	./nexus bar >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
